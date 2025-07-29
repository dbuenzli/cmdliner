(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Commands *)

(* Command info *)

type info = Cmdliner_info.Cmd.t
let info = Cmdliner_info.Cmd.make

type 'a t =
| Cmd of info * 'a Cmdliner_term.parser
| Group of info * ('a Cmdliner_term.parser option * 'a t list)

let get_info = function Cmd (i, _) | Group (i, _) -> i
let children_infos = function
| Cmd _ -> [] | Group (_, (_, cs)) -> List.map get_info cs

let make i t =
  let info = Cmdliner_info.Cmd.add_args i (Cmdliner_term.argset t) in
  Cmd (info, Cmdliner_term.parser t)

let v = make

let group ?default i cmds =
  let args, parser = match default with
  | None -> None, None
  | Some t -> Some (Cmdliner_term.argset t), Some (Cmdliner_term.parser t)
  in
  let children = List.map get_info cmds in
  let i = Cmdliner_info.Cmd.with_children i ~args ~children in
  Group (i, (parser, cmds))

let name c = Cmdliner_info.Cmd.name (get_info c)

let name_trie cmds =
  let add acc cmd =
    let i = get_info cmd in
    let name = Cmdliner_info.Cmd.name i in
    match Cmdliner_trie.add acc name cmd with
    | `New t -> t
    | `Replaced (cmd', _) ->
        let i' = get_info cmd' and kind = "command" in
        invalid_arg @@
        Cmdliner_base.err_multi_def ~kind name Cmdliner_info.Cmd.doc i i'
  in
  List.fold_left add Cmdliner_trie.empty cmds
