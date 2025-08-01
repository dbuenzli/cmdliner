(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Output protocol

   This is a bit ugly we have logic and rendering intermingled. *)

let pp_line ppf s = Cmdliner_base.Fmt.(string ppf s; cut ppf ())
let pp_group ppf s = pp_line ppf "group"; pp_line ppf s
let pp_item ppf ~prefix (name, doc) =
  if Cmdliner_base.string_starts_with ~prefix name then begin
    pp_line ppf "item";
    pp_line ppf name;
    Cmdliner_base.Fmt.(pf ppf "@[%a@]@," text doc);
    pp_line ppf "item-end";
  end

let pp_opt ~err_ppf ~subst ~prefix ppf arg_info _ =
  (* XXX should we rather list a single name ? *)
  let names = Cmdliner_info.Arg.opt_names arg_info in
  let subst = Cmdliner_info.Arg.doclang_subst ~subst arg_info in
  let doc = Cmdliner_info.Arg.styled_doc ~errs:err_ppf ~subst arg_info in
  List.iter (fun name -> pp_item ppf ~prefix (name, doc)) names

let pp_opt_names ~err_ppf ~subst ~prefix ppf cmd =
  let info = Cmdliner_cmd.get_info cmd in
  let set = Cmdliner_info.Cmd.args info in
  if not (Cmdliner_info.Arg.Set.is_empty set) then begin
    let arg_infos = Cmdliner_info.Cmd.args info in
    pp_group ppf "Options";
    Cmdliner_info.Arg.Set.iter (pp_opt ~err_ppf ~subst ~prefix ppf) arg_infos
  end

let pp_arg_values ~after_dashdash ~prefix ppf comp =
  if after_dashdash && Cmdliner_base.Completion.restart comp
  then pp_line ppf "restart" else
  let items = Cmdliner_base.Completion.complete comp prefix in
  let comp_files = Cmdliner_base.Completion.files comp in
  let comp_dirs = Cmdliner_base.Completion.dirs comp in
  if items <> [] || comp_files || comp_dirs then begin
    pp_group ppf "Values";
    List.iter (pp_item ppf ~prefix) items;
    if comp_files then pp_line ppf "files";
    if comp_dirs then pp_line ppf "dirs"
  end

let pp_subcmds ~err_ppf ~subst ~prefix ppf cmd =
  pp_group ppf "Subcommands";
  let complete_cmd cmd =
    let name = Cmdliner_info.Cmd.name cmd in
    (* FIXME subst is wrong here. *)
    let doc = Cmdliner_info.Cmd.styled_doc ~errs:err_ppf ~subst cmd in
    pp_item ppf ~prefix (name, doc)
  in
  List.iter complete_cmd (Cmdliner_cmd.get_children_infos cmd)

let vnum = 1 (* Protocol version number *)

let output ~out_ppf ~err_ppf ei cmd_args_info cmd comp =
  let subst = Cmdliner_info.Eval.doclang_subst ei in
  let after_dashdash = comp.Cmdliner_info.Completion.after_dashdash in
  let prefix = comp.Cmdliner_info.Completion.prefix in
  let pp_arg_value ppf arg_info =
    begin match Cmdliner_info.Arg.Set.find_opt arg_info cmd_args_info with
    | None -> ()
    | Some (V comp) -> pp_arg_values ~after_dashdash ~prefix ppf comp
    end;
  in
  let pp ppf () =
    begin match comp.Cmdliner_info.Completion.kind with
    | Opt_value arg_info -> pp_arg_value ppf arg_info
    | Opt_name_or_pos_value arg_info ->
        pp_arg_value ppf arg_info;
        if not after_dashdash
        then pp_opt_names ~err_ppf ~subst ~prefix ppf cmd
    | Opt_name ->
        if not after_dashdash
        then pp_opt_names ~err_ppf ~subst ~prefix ppf cmd;
    end;
    if comp.subcmds
    then pp_subcmds ~err_ppf ~subst ~prefix ppf cmd
  in
  Cmdliner_base.Fmt.pf out_ppf "@[<v>%d@,%a@]@?" vnum pp ()
