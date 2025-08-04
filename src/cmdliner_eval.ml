(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a eval_ok = [ `Ok of 'a | `Version | `Help ]
type eval_error = [ `Parse | `Term | `Exn ]
type 'a eval_exit = [ `Ok of 'a  | `Exit of Cmdliner_info.Exit.code ]

type 'a complete =
  Cmdliner_info.Arg.Set.t * 'a Cmdliner_cmd.t * Cmdliner_info.Complete.t

type eval_result_error =
  [ Cmdliner_term.term_escape
  | `Exn of exn * Printexc.raw_backtrace
  | `Parse of string
  | `Std_help of Cmdliner_manpage.format
  | `Std_version ]

type 'a eval_result =
  ('a, [ eval_result_error | `Complete of 'a complete]) result

let err_help s = "Term error, help requested for unknown command " ^ s
let err_argv = "argv array must have at least one element"

let add_stdopts ei =
  let docs = Cmdliner_info.Cmd_info.stdopts_docs (Cmdliner_info.Eval.cmd ei) in
  let vargs, vers =
    match Cmdliner_info.Cmd_info.version (Cmdliner_info.Eval.main ei) with
    | None -> Cmdliner_info.Arg.Set.empty, None
    | Some _ ->
        let vers = Cmdliner_arg.stdopt_version ~docs in
        (Cmdliner_term.argset vers), Some vers
  in
  let help = Cmdliner_arg.stdopt_help ~docs in
  let args = Cmdliner_info.Arg.Set.union vargs (Cmdliner_term.argset help) in
  let cmd = Cmdliner_info.Cmd_info.add_args (Cmdliner_info.Eval.cmd ei) args in
  help, vers, Cmdliner_info.Eval.with_cmd ei cmd

let run_parser ~catch ei cl f =
  try (f ei cl :> ('a, eval_result_error) result) with
  | exn when catch ->
      let bt = Printexc.get_raw_backtrace () in
      Error (`Exn (exn, bt))

let try_eval_stdopts ~catch ei cl help version : 'a eval_result option =
  match run_parser ~catch ei cl (Cmdliner_term.parser help) with
  | Ok (Some fmt) -> Some (Error (`Std_help fmt))
  | Error (`Parse _) ->
      (* only [FMT] errored, there was a `--help`, show help anyways *)
      Some (Error (`Std_help `Auto))
  | Error _ as err -> (Some err :> 'a eval_result option)
  | Ok None ->
      match version with
      | None -> None
      | Some version ->
          match (run_parser ~catch ei cl (Cmdliner_term.parser version))
          with
          | Ok false -> None
          | Ok true -> Some (Error (`Std_version))
          | Error _ as err -> (Some err :> 'a eval_result option)

let do_help ~env help_ppf err_ppf ei fmt cmd_name =
  let ei = match cmd_name with
  | None (* help of main command requested *)  ->
      let env _ = assert false in
      let cmd = Cmdliner_info.Eval.main ei in
      let ei' = Cmdliner_info.Eval.make ~cmd ~ancestors:[] ~env ~err_ppf in
      begin match Cmdliner_info.Eval.ancestors ei with
      | [] -> (* [ei] is an evaluation of main, [cmd] has stdopts *) ei'
      | _ -> let _, _, ei = add_stdopts ei' in ei
      end
  | Some cmd ->
      try
        (* For now we simply keep backward compat. [cmd] should be
           a name from main's children. *)
        let main = Cmdliner_info.Eval.main ei in
        let is_cmd t = Cmdliner_info.Cmd_info.name t = cmd in
        let children = Cmdliner_info.Cmd_info.children main in
        let cmd = List.find is_cmd children in
        let _, _, ei = add_stdopts (Cmdliner_info.Eval.with_cmd ei cmd) in
        ei
      with Not_found -> invalid_arg (err_help cmd)
  in
  Cmdliner_docgen.pp_man ~env ~errs:err_ppf fmt help_ppf ei

let do_result ~env help_ppf err_ppf ei = function
| Ok v -> Ok (`Ok v)
| Error res ->
    match res with
    | `Std_help fmt ->
        Cmdliner_docgen.pp_man ~env ~errs:err_ppf fmt help_ppf ei; Ok `Help
    | `Std_version ->
        Cmdliner_msg.pp_version help_ppf ei; Ok `Version
    | `Parse err ->
        Cmdliner_msg.pp_usage_and_err err_ppf ei ~err; Error `Parse
    | `Complete (cmd_args_info, cmd, comp) ->
        Cmdliner_completion.output
          ~out_ppf:help_ppf ~err_ppf ei cmd_args_info cmd comp; Ok `Help
    | `Help (fmt, cmd_name) ->
        do_help ~env help_ppf err_ppf ei fmt cmd_name; Ok `Help
    | `Exn (e, bt) ->
        Cmdliner_msg.pp_backtrace err_ppf ei e bt; (Error `Exn)
    | `Error (usage, err) ->
        (if usage
         then Cmdliner_msg.pp_usage_and_err err_ppf ei ~err
         else Cmdliner_msg.pp_err err_ppf ei ~err);
        Error `Term

let do_deprecated_msgs ~env err_ppf cl ei =
  let cmd_info = Cmdliner_info.Eval.cmd ei in
  let deprecated = Cmdliner_cline.deprecated ~env cl in
  match Cmdliner_info.Cmd_info.deprecated cmd_info, deprecated with
  | None, [] -> ()
  | depr_cmd, deprs ->
      let open Cmdliner_base in
      let pp_sep ppf () =
        if Option.is_some depr_cmd && deprs <> [] then Fmt.cut ppf ();
      in
      let subst = Cmdliner_info.Eval.doclang_subst ei in
      let pp_cmd_msg ppf cmd =
        match
          Cmdliner_info.Cmd_info.styled_deprecated ~subst ~errs:err_ppf cmd
        with
        | "" -> ()
        | msg ->
            let name = Cmdliner_info.Cmd_info.name cmd in
            Fmt.pf ppf "@[%a command %a:@[ %a@]@]"
              Fmt.deprecated () Fmt.code_or_quote name Fmt.styled_text msg
      in
      let pp_deprs = Fmt.list (Cmdliner_cline.pp_deprecated ~subst) in
      Fmt.pf err_ppf "@[%a @[<v>%a%a%a@]@]@."
        Cmdliner_msg.pp_exec_msg ei pp_cmd_msg cmd_info pp_sep () pp_deprs deprs

let find_cmd_and_parser ~legacy_prefixes ~for_completion args cmd =
  (* This finds the command to use if it's a group and [for_completion]
     is [true] whether we may need to add the subcommand names to the
     completions. *)
  let stop ~ancestors ~cmd args = match (cmd : 'a Cmdliner_cmd.t) with
  | Cmd (_, parser) -> ancestors, cmd, args, Ok parser
  | Group (_, (Some parser, _)) -> ancestors, cmd, args, Ok parser
  | Group (_, (None, children)) ->
      let dom = Cmdliner_cmd.list_names children in
      let err = Cmdliner_msg.err_cmd_missing ~dom in
      let try_stdopts = true in
      ancestors, cmd, args, Error (`Parse (try_stdopts, err))
  in
  let rec loop ~ancestors ~current_cmd = function
  | "--" :: _ | [] as args -> stop ~ancestors ~cmd:current_cmd args
  | arg :: _ as args when for_completion &&
                          Cmdliner_cline.has_complete_prefix arg ->
      begin match current_cmd with
      | Cmd _ -> (* arg completion *) stop ~ancestors ~cmd:current_cmd args
      | Group (_, (parser, _))  ->
          let is_opt = Cmdliner_cline.(is_opt (get_token_to_complete arg)) in
          if not is_opt then ancestors, current_cmd, args, Error `Complete else
          stop ~ancestors ~cmd:current_cmd args
      end
  | arg :: _ as args when Cmdliner_cline.is_opt arg ->
      stop ~ancestors ~cmd:current_cmd args
  | arg :: rest as args ->
      match current_cmd with
      | Cmd (i, parser) -> ancestors, current_cmd, args, Ok parser
      | Group (i, (_, children)) ->
          let cmd_index = Cmdliner_cmd.name_trie children in
          match Cmdliner_trie.find ~legacy_prefixes cmd_index arg with
          | Ok cmd -> loop ~ancestors:(i :: ancestors) ~current_cmd:cmd rest
          | Error `Not_found ->
              let all = Cmdliner_trie.ambiguities cmd_index "" in
              let hints = Cmdliner_base.suggest arg all in
              let dom = Cmdliner_cmd.list_names children in
              let kind = "command" in
              let err = Cmdliner_base.err_unknown ~kind ~dom ~hints arg in
              let try_stdopts =
                (* When we users writes cmd no_such_cmd --help it's
                   better to show the unknown command error message
                   rather than get into the help of the parent
                   command. Otherwise one gets confused into thinking
                   the command exists and/or annoyed not be reading
                   the right man page. *)
                false
              in
              ancestors, current_cmd, args, Error (`Parse (try_stdopts, err))
          | Error `Ambiguous (* Only on legacy prefixes *)  ->
              let ambs = Cmdliner_trie.ambiguities cmd_index arg in
              let ambs = List.sort compare ambs in
              let err = Cmdliner_base.err_ambiguous ~kind:"command" arg ~ambs in
              let try_stdopts = false in
              ancestors, current_cmd, args, Error (`Parse (try_stdopts, err))
  in
  loop ~ancestors:[] ~current_cmd:cmd args

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
  let for_completion, args = cli_args_of_argv argv in
  let ancestors, cmd, args, parser =
    find_cmd_and_parser ~legacy_prefixes ~for_completion args cmd
  in
  let help, version, ei =
    let cmd_info = Cmdliner_cmd.get_info cmd in
    let ei = Cmdliner_info.Eval.make ~cmd:cmd_info ~ancestors ~env ~err_ppf in
    add_stdopts ei
  in
  let cmd_args_info = Cmdliner_info.Cmd_info.args (Cmdliner_info.Eval.cmd ei) in
  let cline =
    Cmdliner_cline.create ~legacy_prefixes ~for_completion cmd_args_info args
  in
  let res = match parser with
  | Error (`Parse (try_stdopts, msg)) ->
      (* Command lookup error, we may still prioritize stdargs *)
      begin match cline with
      | `Complete comp -> Error (`Complete (cmd_args_info, cmd, comp))
      | `Error (_, cl) | `Ok cl ->
          let stdopts =
            if try_stdopts then try_eval_stdopts ~catch ei cl help version else
            None
          in
          begin match stdopts with
          | None -> Error (`Error (true, msg))
          | Some e -> e
          end
      end
  | Error `Complete ->
      begin match cline with
      | `Complete comp ->
          let comp = Cmdliner_info.Complete.add_subcmds comp in
          Error (`Complete (cmd_args_info, cmd, comp))
      | `Ok _ | `Error _ -> assert false
      end
  | Ok parser ->
      begin match cline with
      | `Complete comp -> Error (`Complete (cmd_args_info, cmd, comp))
      | `Error (e, cl) ->
          begin match try_eval_stdopts ~catch ei cl help version with
          | Some e -> e
          | None -> Error (`Error (true, e))
          end
      | `Ok cl ->
          match try_eval_stdopts ~catch ei cl help version with
          | Some e -> e
          | None ->
              do_deprecated_msgs ~env err_ppf cl ei;
              (run_parser ~catch ei cl parser :> 'a eval_result)
      end
  in
  do_result ~env help_ppf err_ppf ei res

let eval_peek_opts
    ?(version_opt = false) ?(env = Sys.getenv_opt) ?(argv = Sys.argv) t
  : 'a option * ('a eval_ok, eval_error) result
  =
  let legacy_prefixes = Cmdliner_trie.legacy_prefixes ~env in
  let for_completion, args = cli_args_of_argv argv in
  let version = if version_opt then Some "dummy" else None in
  let cmd_info, parser =
    let args, parser = Cmdliner_term.argset t, Cmdliner_term.parser t in
    let cmd_info = Cmdliner_info.Cmd_info.make ?version "dummy" in
    Cmdliner_info.Cmd_info.add_args cmd_info args, parser
  in
  let help, version, ei =
    let err_ppf = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ()) in
    let ancestors = [] in
    let ei = Cmdliner_info.Eval.make ~cmd:cmd_info ~ancestors ~env ~err_ppf in
    add_stdopts ei
  in
  let cmd_arg_infos = Cmdliner_info.Cmd_info.args (Cmdliner_info.Eval.cmd ei) in
  let cline =
    Cmdliner_cline.create
      ~peek_opts:true ~legacy_prefixes ~for_completion cmd_arg_infos args
  in
  let v, ret = match cline with
  | `Complete comp ->
      let cmd = Cmdliner_cmd.make cmd_info t in
      None, (Error (`Complete (cmd_arg_infos, cmd, comp)))
  | `Error (e, cl) ->
      begin match try_eval_stdopts ~catch:true ei cl help version with
      | Some e -> None, e
      | None -> None, Error (`Error (true, e))
      end
  | `Ok cl ->
      let ret = run_parser ~catch:true ei cl parser in
      let v = match ret with Ok v -> Some v | Error _ -> None in
      begin match try_eval_stdopts ~catch:true ei cl help version with
      | Some e -> v, e
      | None -> v, (ret :> 'a eval_result)
      end
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
