(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a eval_ok = [ `Ok of 'a | `Version | `Help ]
type eval_error = [ `Parse | `Term | `Exn ]
type 'a eval_exit = [ `Ok of 'a  | `Exit of Cmdliner_info.Exit.code ]

let err_help s = "Term error, help requested for unknown command " ^ s
let err_argv = "argv array must have at least one element"

let add_stdopts ei =
  let docs = Cmdliner_info.Cmd.stdopts_docs (Cmdliner_info.Eval.cmd ei) in
  let vargs, vers =
    match Cmdliner_info.Cmd.version (Cmdliner_info.Eval.main ei) with
    | None -> Cmdliner_info.Arg.Set.empty, None
    | Some _ ->
        let vers = Cmdliner_arg.stdopt_version ~docs in
        (Cmdliner_term.argset vers), Some vers
  in
  let help = Cmdliner_arg.stdopt_help ~docs in
  let args = Cmdliner_info.Arg.Set.union vargs (Cmdliner_term.argset help) in
  let cmd = Cmdliner_info.Cmd.add_args (Cmdliner_info.Eval.cmd ei) args in
  help, vers, Cmdliner_info.Eval.with_cmd ei cmd

let parse_error_term err ei cl = Error (`Parse err)

type complete =
  Cmdliner_info.Arg.Set.t * Cmdliner_cmd.info * Cmdliner_cmd.info list *
  Cmdliner_cline.completion

type 'a eval_result =
  ('a, [ Cmdliner_term.term_escape
       | `Exn of exn * Printexc.raw_backtrace
       | `Parse of string
       | `Std_help of Cmdliner_manpage.format
       | `Std_version
       | `Complete of complete]) result

let run_parser ~catch ei cl f = try (f ei cl :> 'a eval_result) with
| exn when catch ->
    let bt = Printexc.get_raw_backtrace () in
    Error (`Exn (exn, bt))

let try_eval_stdopts ~catch ei cl help version =
  match run_parser ~catch ei cl (Cmdliner_term.parser help) with
  | Ok (Some fmt) -> Some (Error (`Std_help fmt))
  | Error (`Complete _) -> assert false
  | Error (`Parse _) ->
      (* only [FMT] errored, there was a `--help`, show help anyways *)
      Some (Error (`Std_help `Auto))
  | Error _ as err -> Some err
  | Ok None ->
      match version with
      | None -> None
      | Some version ->
          match run_parser ~catch ei cl (Cmdliner_term.parser version) with
          | Ok false -> None
          | Ok true -> Some (Error (`Std_version))
          | Error _ as err -> Some err

let do_help ~env help_ppf err_ppf ei fmt cmd =
  let ei = match cmd with
  | None (* help of main command requested *)  ->
      let env _ = assert false in
      let cmd = Cmdliner_info.Eval.main ei in
      let ei' = Cmdliner_info.Eval.make ~cmd ~parents:[] ~env ~err_ppf in
      begin match Cmdliner_info.Eval.parents ei with
      | [] -> (* [ei] is an evaluation of main, [cmd] has stdopts *) ei'
      | _ -> let _, _, ei = add_stdopts ei' in ei
      end
  | Some cmd ->
      try
        (* For now we simply keep backward compat. [cmd] should be
           a name from main's children. *)
        let main = Cmdliner_info.Eval.main ei in
        let is_cmd t = Cmdliner_info.Cmd.name t = cmd in
        let children = Cmdliner_info.Cmd.children main in
        let cmd = List.find is_cmd children in
        let _, _, ei = add_stdopts (Cmdliner_info.Eval.with_cmd ei cmd) in
        ei
      with Not_found -> invalid_arg (err_help cmd)
  in
  Cmdliner_docgen.pp_man ~env ~errs:err_ppf fmt help_ppf ei

let do_completion help_ppf err_ppf ei args cmd cmd_children comp =
  let prefix = comp.Cmdliner_cline.prefix in
  let after_dashdash = comp.Cmdliner_cline.after_dashdash in
  let subst = Cmdliner_info.Eval.doclang_subst ei in
  let pp_line ppf s = Cmdliner_base.Fmt.(string ppf s; cut ppf ()) in
  let pp_group ppf s = pp_line ppf "group"; pp_line ppf s in
  let pp_item ppf ~prefix (name, doc) =
    if Cmdliner_base.string_has_prefix ~prefix name then begin
      pp_line ppf "item";
      pp_line ppf name;
      Cmdliner_base.Fmt.(pf ppf "@[%a@]@," text doc);
      pp_line ppf "item-end";
    end
  in
  let pp_option ppf arginfo _ =
    (* XXX should we rather list a single name ? *)
    let names = Cmdliner_info.Arg.opt_names arginfo in
    let subst = Cmdliner_info.Arg.doclang_subst ~subst arginfo in
    let doc = Cmdliner_info.Arg.styled_doc ~errs:err_ppf ~subst arginfo in
    List.iter (fun name -> pp_item ppf ~prefix (name, doc)) names
  in
  let pp_complete_opt_names ppf cmd =
    let set = Cmdliner_info.Cmd.args cmd in
    if not (Cmdliner_info.Arg.Set.is_empty set) then begin
      pp_group ppf "Options";
      Cmdliner_info.Arg.Set.iter (pp_option ppf) (Cmdliner_info.Cmd.args cmd)
    end
  in
  let pp_complete_arg_values ~after_dashdash ppf arg =
    match Cmdliner_info.Arg.Set.find_opt arg args with
    | None -> ()
    | Some (V comp) ->
        if after_dashdash && Cmdliner_base.Completion.restart comp
        then pp_line ppf "restart"
        else
        let items = Cmdliner_base.Completion.complete comp prefix in
        let comp_files = Cmdliner_base.Completion.files comp in
        let comp_dirs = Cmdliner_base.Completion.dirs comp in
        if items <> [] || comp_files || comp_dirs then begin
          pp_group ppf "Values";
          List.iter (pp_item ppf ~prefix) items;
          if comp_files then pp_line ppf "files";
          if comp_dirs then pp_line ppf "dirs"
        end
  in
  let pp_complete_subcommands ppf cmd_children =
    pp_group ppf "Subcommands";
    let complete_cmd cmd =
      let name = Cmdliner_info.Cmd.name cmd in
      (* FIXME subst is wrong here. *)
      let doc = Cmdliner_info.Cmd.styled_doc ~errs:err_ppf ~subst cmd in
      pp_item ppf ~prefix (name, doc)
    in
    List.iter complete_cmd cmd_children
  in
  let pp_completions ppf () =
    begin match comp.kind with
    | `Opt a -> pp_complete_arg_values ~after_dashdash ppf a
    | `Arg a ->
        pp_complete_arg_values ~after_dashdash ppf a;
        if not after_dashdash then pp_complete_opt_names ppf cmd
    | `Any ->
        if not comp.after_dashdash then pp_complete_opt_names ppf cmd;
    end;
    if cmd_children <> []
    then pp_complete_subcommands ppf cmd_children
  in
  let vnum = 1 in
  Cmdliner_base.Fmt.pf help_ppf "@[<v>%d@,%a@]@?" vnum pp_completions ()

let do_result ~env help_ppf err_ppf ei = function
| Ok v -> Ok (`Ok v)
| Error res ->
    match res with
    | `Std_help fmt ->
        Cmdliner_docgen.pp_man ~env ~errs:err_ppf fmt help_ppf ei; Ok `Help
    | `Std_version ->
        Cmdliner_msg.pp_version help_ppf ei; Ok `Version
    | `Parse err ->
        Cmdliner_msg.pp_usage_and_err err_ppf ei ~err;
        Error `Parse
    | `Complete (args, cmd, cmd_children, comp) ->
        do_completion help_ppf err_ppf ei
          args cmd cmd_children comp; Ok `Help
    | `Help (fmt, cmd) -> do_help ~env help_ppf err_ppf ei fmt cmd; Ok `Help
    | `Exn (e, bt) -> Cmdliner_msg.pp_backtrace err_ppf ei e bt; (Error `Exn)
    | `Error (usage, err) ->
        (if usage
         then Cmdliner_msg.pp_usage_and_err err_ppf ei ~err
         else Cmdliner_msg.pp_err err_ppf ei ~err);
        Error `Term

let do_deprecated_msgs ~env err_ppf cl ei =
  let cmd = Cmdliner_info.Eval.cmd ei in
  let deprecated = Cmdliner_cline.deprecated ~env cl in
  match Cmdliner_info.Cmd.deprecated cmd, deprecated with
  | None, [] -> ()
  | depr_cmd, deprs ->
      let open Cmdliner_base in
      let pp_sep ppf () =
        if Option.is_some depr_cmd && deprs <> [] then Fmt.cut ppf ();
      in
      let subst = Cmdliner_info.Eval.doclang_subst ei in
      let pp_cmd_msg ppf cmd =
        match Cmdliner_info.Cmd.styled_deprecated ~subst ~errs:err_ppf cmd with
        | "" -> ()
        | msg ->
            let name = Cmdliner_info.Cmd.name cmd in
            Fmt.pf ppf "@[%a command %a:@[ %a@]@]"
              Fmt.deprecated () Fmt.code_or_quote name Fmt.styled_text msg
      in
      let pp_deprs = Fmt.list (Cmdliner_cline.pp_deprecated ~subst) in
      Fmt.pf err_ppf "@[%a @[<v>%a%a%a@]@]@."
        Cmdliner_msg.pp_exec_msg ei pp_cmd_msg cmd pp_sep () pp_deprs deprs

let cmd_name_dom cmds =
  let cmd_name c = Cmdliner_info.Cmd.name (Cmdliner_cmd.get_info c) in
  List.sort String.compare (List.rev_map cmd_name cmds)

let find_parser ~legacy_prefixes ~for_completion args cmd =
  let stop args parents cmd =
    match (cmd : 'a Cmdliner_cmd.t) with
    | Cmd (i, parser) -> args, i, parents, Ok parser
    | Group (i, (Some parser, _)) -> args, i, parents, Ok parser
    | Group (i, (None, children)) ->
        let dom = cmd_name_dom children in
        let err = Cmdliner_msg.err_cmd_missing ~dom in
        let try_stdopts = true in
        args, i, parents, Error (`Parse (try_stdopts, err))
  in
  let rec loop parents cmd = function
  | ("--" :: _ | [] as rest) -> stop rest parents cmd
  | (arg :: args) as rest
    when for_completion && Cmdliner_cline.has_complete_prefix arg ->
      begin match cmd with
      | Cmd _ -> (* arg completion *) stop rest parents cmd
      | Group (i, group) -> rest, i, parents, Error (`Complete group)
      end
  | (arg :: _ as rest) when Cmdliner_cline.is_opt arg -> stop rest parents cmd
  | (arg :: args) as rest ->
      match cmd with
      | Cmd (i, parser) -> rest, i, parents, Ok parser
      | Group (i, (_, children)) ->
          let cmd_index = Cmdliner_cmd.name_trie children in
          match Cmdliner_trie.find ~legacy_prefixes cmd_index arg with
          | Ok cmd -> loop (i :: parents) cmd args
          | Error `Not_found ->
              let all = Cmdliner_trie.ambiguities cmd_index "" in
              let hints = Cmdliner_base.suggest arg all in
              let dom = cmd_name_dom children in
              let kind = "command" in
              let err = Cmdliner_base.err_unknown ~kind ~dom ~hints arg in
              let try_stdopts =
                (* When we users writes cmd no_such_cmd --help it's
                   better to show the unknown command error message
                   rather than get into the help of the parent command.
                   Otherwise one gets confused into thinking the
                   command exists and/or annoyed not be reading the right
                   man page. *)
                false
              in
              rest, i, parents, Error (`Parse (try_stdopts, err))
          | Error `Ambiguous (* Only on legacy prefixes *)  ->
              let ambs = Cmdliner_trie.ambiguities cmd_index arg in
              let ambs = List.sort compare ambs in
              let err = Cmdliner_base.err_ambiguous ~kind:"command" arg ~ambs in
              let try_stdopts = true in
              rest, i, parents, Error (`Parse (try_stdopts, err))
  in
  loop [] cmd args

let cli_args_of_argv argv = match Array.to_list argv with
| exec :: "--__complete" :: args -> true, args
| exec :: args -> false, args
| [] -> invalid_arg err_argv

let eval_value
    ?help:(help_ppf = Format.std_formatter)
    ?err:(err_ppf = Format.err_formatter)
    ?(catch = true) ?(env = Sys.getenv_opt) ?(argv = Sys.argv) cmd
  =
  let legacy_prefixes = Cmdliner_trie.legacy_prefixes ~env in
  let for_completion, cli_args = cli_args_of_argv argv in
  let args, cmd, parents, res =
    find_parser ~legacy_prefixes ~for_completion cli_args cmd
  in
  let ei = Cmdliner_info.Eval.make ~cmd ~parents ~env ~err_ppf in
  let help, version, ei = add_stdopts ei in
  let term_args = Cmdliner_info.Cmd.args (Cmdliner_info.Eval.cmd ei) in
  let cline =
    Cmdliner_cline.create ~legacy_prefixes ~for_completion term_args args
  in
  let res = match res with
  | Error (`Parse (try_stdopts, msg)) ->
      (* Command lookup error, we may still prioritize stdargs *)
      begin match cline with
      | `Completion compl -> Error (`Complete (term_args, cmd, [], compl))
      | `Error (_, cl) | `Ok cl ->
          if not try_stdopts then Error (`Error (true, msg)) else
          begin match try_eval_stdopts ~catch ei cl help version with
          | Some e -> e
          | None -> Error (`Error (true, msg))
          end
      end
  | Error (`Complete (default, children)) ->
      let children = List.map Cmdliner_cmd.get_info children in
      begin match cline with
      | `Completion compl -> Error (`Complete (term_args, cmd, children, compl))
      | `Ok _ | `Error _ -> assert false
      end
  | Ok parser ->
      match cline with
      | `Completion compl -> Error (`Complete (term_args, cmd, [], compl))
      | `Error (e, cl) ->
          begin match try_eval_stdopts ~catch ei cl help version with
          | Some e -> e
          | None -> Error (`Error (true, e))
          end
      | `Ok cl ->
          assert (not for_completion);
          match try_eval_stdopts ~catch ei cl help version with
          | Some e -> e
          | None ->
              do_deprecated_msgs ~env err_ppf cl ei;
              run_parser ~catch ei cl parser
  in
  do_result ~env help_ppf err_ppf ei res

let eval_peek_opts
    ?(version_opt = false) ?(env = Sys.getenv_opt) ?(argv = Sys.argv) t
  : 'a option * ('a eval_ok, eval_error) result
  =
  let args, f = Cmdliner_term.argset t, Cmdliner_term.parser t in
  let version = if version_opt then Some "dummy" else None in
  let cmd = Cmdliner_info.Cmd.make ?version "dummy" in
  let cmd = Cmdliner_info.Cmd.add_args cmd args in
  let null_ppf = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ()) in
  let ei = Cmdliner_info.Eval.make ~cmd ~parents:[] ~env ~err_ppf:null_ppf in
  let help, version, ei = add_stdopts ei in
  let term_args = Cmdliner_info.Cmd.args @@ Cmdliner_info.Eval.cmd ei in
  let for_completion, cli_args = cli_args_of_argv argv in
  let legacy_prefixes = Cmdliner_trie.legacy_prefixes ~env in
  let v, ret =
    match
      Cmdliner_cline.create
        ~peek_opts:true ~legacy_prefixes ~for_completion term_args cli_args
    with
    | `Completion arg -> None, (Error (`Complete (term_args, cmd, [], arg)))
    | `Error (e, cl) ->
        begin match try_eval_stdopts ~catch:true ei cl help version with
        | Some e -> None, e
        | None -> None, Error (`Error (true, e))
        end
    | `Ok cl ->
        let ret = run_parser ~catch:true ei cl f in
        let v = match ret with Ok v -> Some v | Error _ -> None in
        match try_eval_stdopts ~catch:true ei cl help version with
        | Some e -> v, e
        | None -> v, ret
  in
  let ret = match ret with
  | Ok v -> Ok (`Ok v)
  | Error `Std_help _ -> Ok `Help
  | Error `Std_version -> Ok `Version
  | Error `Parse _ -> Error `Parse
  | Error `Help _ -> Ok `Help
  | Error `Complete _ -> Ok `Help
  | Error `Exn _ -> Error `Exn
  | Error `Error _ -> Error `Term
  in
  (v, ret)

let exit_status_of_result ?(term_err = Cmdliner_info.Exit.cli_error) = function
| Ok (`Ok _ | `Help | `Version) -> Cmdliner_info.Exit.ok
| Error `Term -> term_err
| Error `Parse -> Cmdliner_info.Exit.cli_error
| Error `Exn -> Cmdliner_info.Exit.internal_error

let eval_value' ?help ?err ?catch ?env ?argv ?term_err cmd =
  match eval_value ?help ?err ?catch ?env ?argv cmd with
  | Ok (`Ok _ as v) -> v
  | ret -> `Exit (exit_status_of_result ?term_err ret)

let eval ?help ?err ?catch ?env ?argv ?term_err cmd =
  exit_status_of_result ?term_err @@
  eval_value ?help ?err ?catch ?env ?argv cmd

let eval' ?help ?err ?catch ?env ?argv ?term_err cmd =
  match eval_value ?help ?err ?catch ?env ?argv cmd with
  | Ok (`Ok c) -> c
  | r -> exit_status_of_result ?term_err r

let pp_err ppf cmd ~msg =
  (* Here instead of Cmdliner_msgs to avoid circular dep *)
  let name = Cmdliner_cmd.name cmd in
  Cmdliner_base.Fmt.pf ppf "%s: @[%a@]@." name Cmdliner_base.Fmt.lines msg

let eval_result
    ?help ?(err = Format.err_formatter) ?catch ?env ?argv ?term_err cmd
  =
  match eval_value ?help ~err ?catch ?env ?argv cmd with
  | Ok (`Ok (Error msg)) -> pp_err err cmd ~msg; Cmdliner_info.Exit.some_error
  | r -> exit_status_of_result ?term_err r

let eval_result'
    ?help ?(err = Format.err_formatter) ?catch ?env ?argv ?term_err cmd
  =
  match eval_value ?help ~err ?catch ?env ?argv cmd with
  | Ok (`Ok (Ok c)) -> c
  | Ok (`Ok (Error msg)) -> pp_err err cmd ~msg; Cmdliner_info.Exit.some_error
  | r -> exit_status_of_result ?term_err r
