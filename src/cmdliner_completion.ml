(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Output protocol  *)

let cons_if b v l = if b then v :: l else l

type dir =
| Dirs | Files | Group of string * (string * string) list
| Restart | Message of string

let pp_protocol ppf dirs =
  let pp_line ppf s = Cmdliner_base.Fmt.(string ppf s; cut ppf ()) in
  let pp_text ppf s = Cmdliner_base.Fmt.(pf ppf "@[%a@]@," styled_text s) in
  let vnum = 1 (* Protocol version number *) in
  let pp_item ppf (name, doc) =
    pp_line ppf "item";
    pp_line ppf name; pp_text ppf doc;
    pp_line ppf "item-end";
  in
  let pp_dir ppf = function
  | Dirs -> pp_line ppf "dirs"
  | Files -> pp_line ppf "files"
  | Restart -> pp_line ppf "restart"
  | Group (name, items) ->
      pp_line ppf "group";
      pp_line ppf name;
      Cmdliner_base.Fmt.(list ~sep:nop pp_item) ppf items;
  | Message msg ->
      pp_line ppf "message"; pp_text ppf msg; pp_line ppf "message-end"
  in
  Cmdliner_base.Fmt.pf ppf "@[<v>%d@,%a@]" vnum
    Cmdliner_base.Fmt.(list ~sep:nop pp_dir) dirs

let add_subcommands_group ~err_ppf ~subst cmd comp directives =
  if not (Cmdliner_def.Complete.subcmds comp) then directives else
  let prefix = Cmdliner_def.Complete.prefix comp in
  let maybe_item cmd =
    let name = Cmdliner_def.Cmd_info.name cmd in
    if not (Cmdliner_base.string_starts_with ~prefix name) then None else
    (* FIXME subst is wrong here. *)
    let doc = Cmdliner_def.Cmd_info.styled_doc ~errs:err_ppf ~subst cmd in
    Some (name, doc)
  in
  let subcmds = Cmdliner_cmd.get_children_infos cmd in
  (Group ("Subcommands", List.filter_map maybe_item subcmds)) :: directives

let add_options_group ~err_ppf ~subst cmd comp directives =
  let prefix = Cmdliner_def.Complete.prefix comp in
  let maybe_items arg_info =
    let names = Cmdliner_def.Arg_info.opt_names arg_info in
    let subst = Cmdliner_def.Arg_info.doclang_subst ~subst arg_info in
    let doc = Cmdliner_def.Arg_info.styled_doc ~errs:err_ppf ~subst arg_info in
    let add_name n =
      if not (Cmdliner_base.string_starts_with ~prefix n) then None else
      Some (n, doc)
    in
    List.filter_map add_name names
  in
  let maybe_opt = prefix = "" || prefix.[0] = '-' in
  if Cmdliner_def.Complete.after_dashdash comp || not maybe_opt
  then directives else
  let info = Cmdliner_cmd.get_info cmd in
  let set = Cmdliner_def.Cmd_info.args info in
  if Cmdliner_def.Arg_info.Set.is_empty set then directives else
  let options = Cmdliner_def.Arg_info.Set.elements set in
  Group ("Options", List.concat (List.map maybe_items options)) :: directives

let add_argument_value_directives directives comp =
  let Directives (pp, ds) = Cmdliner_def.Complete.directives comp in
  match ds with
  | Error msg -> `Directives [Message msg]
  | Ok ds ->
      let rec loop values msgs ~files ~dirs ~restart ~raw = function
      | [] ->
          begin match raw with
          | Some r -> `Raw r
          | None ->
              if Cmdliner_def.Complete.after_dashdash comp && restart
              then `Directives [Restart] else
              let dd =
                cons_if dirs Dirs @@
                cons_if files Files @@
                cons_if (values <> []) (Group ("Values", List.rev values)) []
              in
              `Directives (List.rev_append msgs (List.rev_append dd directives))
          end
      | d :: ds ->
          match d with
          | Cmdliner_def.Arg_completion.String (s, doc) ->
              loop ((s, doc) :: values) msgs ~files ~dirs ~restart ~raw ds
          | Value (v, doc) ->
              let s = Cmdliner_base.Fmt.str "@[<h>%a@]" pp v in
              loop ((s, doc) :: values) msgs ~files ~dirs ~restart ~raw ds
          | Files -> loop values msgs ~files:true ~dirs ~restart ~raw ds
          | Dirs -> loop values msgs ~files ~dirs:true ~restart ~raw ds
          | Restart -> loop values msgs ~files ~dirs ~restart:true ~raw ds
          | Message msg ->
              loop values (Message msg :: msgs) ~files ~dirs ~restart ~raw ds
          | Raw r -> loop values msgs ~files ~dirs ~restart ~raw:(Some r) ds
      in
      loop [] [] ~files:false ~dirs:false ~restart:false ~raw:None ds

let output ~out_ppf ~err_ppf ei cmd_args_info cmd comp =
  let subst = Cmdliner_def.Eval.doclang_subst ei in
  let dirs = add_subcommands_group ~err_ppf ~subst cmd comp [] in
  let res = match Cmdliner_def.Complete.kind comp with
  | Opt_value _arg_info ->
      add_argument_value_directives dirs comp
  | Opt_name_or_pos_value _arg_info ->
      let dirs = add_options_group ~err_ppf ~subst cmd comp dirs in
      add_argument_value_directives dirs comp
  | Opt_name ->
      `Directives (add_options_group ~err_ppf ~subst cmd comp dirs)
  in
  match res with
  | `Raw raw -> Cmdliner_base.Fmt.pf out_ppf "%s@?" raw
  | `Directives dirs -> Cmdliner_base.Fmt.pf out_ppf "%a@?" pp_protocol dirs
