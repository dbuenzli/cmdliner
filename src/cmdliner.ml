(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Result

module Manpage = Cmdliner_manpage
module Arg = Cmdliner_arg

let add_stdopts ei =
  let docs = Cmdliner_info.(term_stdopts_docs @@ eval_term ei) in
  let args, v_lookup = match Cmdliner_info.(term_version @@ eval_main ei) with
  | None -> Cmdliner_info.Args.empty, None
  | Some _ ->
      let (a, lookup) = Cmdliner_arg.stdopt_version ~docs in
      a, Some lookup
  in
  let args, h_lookup =
    let (a, lookup) = Cmdliner_arg.stdopt_help ~docs in
    Cmdliner_info.Args.union a args, lookup
  in
  let term = Cmdliner_info.(term_add_args (eval_term ei) args) in
  h_lookup, v_lookup, Cmdliner_info.eval_with_term ei term


module Term = struct

  include Cmdliner_term

  (* Deprecated *)

  let man_format = Cmdliner_arg.man_format
  let pure = const

  (* Terms *)

  let ( $ ) = app

  type 'a ret = [ `Ok of 'a | term_escape ]

  let ret (al, v) =
    al, fun ei cl -> match v ei cl with
    | Ok (`Ok v) -> Ok v
    | Ok (`Error _ as err) -> Error err
    | Ok (`Help _ as help) -> Error help
    | Error _ as e -> e

  let main_name =
    Cmdliner_info.Args.empty,
    (fun ei _ -> Ok (Cmdliner_info.(term_name @@ eval_main ei)))

  let choice_names =
    let choice_name t = Cmdliner_info.term_name t in
    Cmdliner_info.Args.empty,
    (fun ei _ -> Ok (List.rev_map choice_name (Cmdliner_info.eval_choices ei)))

  (* Term information *)

  type info = Cmdliner_info.term
  let info = Cmdliner_info.term ~args:Cmdliner_info.Args.empty
  let name ti = Cmdliner_info.term_name ti

  type 'a result =
    [ `Ok of 'a | `Error of [`Parse | `Term | `Exn ] | `Version | `Help ]

  let err_help s = "Term error, help requested for unknown command " ^ s
  let err_argv = "argv array must have at least one element"
  let err_multi_cmd_def name (a, _) (a', _) =
    Cmdliner_base.err_multi_def ~kind:"command" name Cmdliner_info.term_doc a a'

  (* Evaluation *)

  let remove_exec argv =
    try List.tl (Array.to_list argv) with Failure _ -> invalid_arg err_argv

  let eval_help_cmd help_ppf ei fmt cmd =
    let ei = match cmd with
    | None -> Cmdliner_info.(eval_with_term ei @@ eval_main ei)
    | Some cmd ->
        try
          let is_cmd t = Cmdliner_info.term_name t = cmd in
          let cmd = List.find is_cmd (Cmdliner_info.eval_choices ei) in
          Cmdliner_info.eval_with_term ei cmd
        with Not_found -> invalid_arg (err_help cmd)
    in
    let _, _, ei = add_stdopts ei in
    Cmdliner_docgen.pp_man fmt help_ppf ei; `Help

  let eval_err help_ppf err_ppf ei = function
  | `Help (fmt, cmd) -> eval_help_cmd help_ppf ei fmt cmd
  | `Parse err -> Cmdliner_msg.pp_err_usage err_ppf ei ~err; `Error `Parse
  | `Error (usage, err) ->
      (if usage
       then Cmdliner_msg.pp_err_usage err_ppf ei ~err
       else Cmdliner_msg.pp_err err_ppf ei ~err);
      `Error `Term

  let eval_fun ~catch help_ppf err_ppf ei cl f =
    try match f ei cl with
    | Error err -> eval_err help_ppf err_ppf ei err
    | Ok v -> `Ok v
    with
    | exn when catch ->
        let bt = Printexc.get_backtrace () in
        Cmdliner_msg.pp_backtrace err_ppf ei exn bt; `Error `Exn

  let eval_term ~catch help_ppf err_ppf ei f args =
    let help_arg, vers_arg, ei = add_stdopts ei in
    let term_args = Cmdliner_info.(term_args @@ eval_term ei) in
    match Cmdliner_cline.create term_args args with
    | Error (e, cl) ->
        begin match help_arg ei cl with
        | Error err -> eval_err help_ppf err_ppf ei err
        | Ok (Some fmt) -> Cmdliner_docgen.pp_man fmt help_ppf ei; `Help
        | Ok None -> eval_err help_ppf err_ppf ei (`Error (true, e))
        end
    | Ok cl ->
        match help_arg ei cl with
        | Error err -> eval_err help_ppf err_ppf ei err
        | Ok (Some fmt) -> Cmdliner_docgen.pp_man fmt help_ppf ei; `Help
        | Ok None ->
            match vers_arg with
            | None -> eval_fun ~catch help_ppf err_ppf ei cl f
            | Some v_arg ->
                match v_arg ei cl with
                | Error err -> eval_err help_ppf err_ppf ei err
                | Ok true -> Cmdliner_msg.pp_version help_ppf ei; `Version
                | Ok false -> eval_fun ~catch help_ppf err_ppf ei cl f

  let term_eval_peek_opts ei f args =
    let ret_to_opt = function
    | `Ok v -> Some v | `Error _ -> None | `Help -> None
    in
    let eval_err = function
    | `Help _ -> `Help
    | `Parse _ -> `Error `Parse
    | `Error _ -> `Error `Term
    in
    let eval_fun ei cl f =
      try match f ei cl with
      | Ok v -> `Ok v
      | Error err -> eval_err err
      with e -> `Error `Exn
    in
    let help_arg, vers_arg, ei = add_stdopts ei in
    let term_args = Cmdliner_info.(term_args @@ eval_term ei) in
    match Cmdliner_cline.create ~peek_opts:true term_args args with
    | Error (e, cl) ->
        begin match help_arg ei cl with
        | Ok (Some fmt) -> None, `Help
        | Ok None -> None, eval_err (`Parse e)
        | Error err -> None, eval_err err
        end
    | Ok cl ->
        let ret = eval_fun ei cl f in
        match help_arg ei cl with
        | Error err -> None, eval_err err
        | Ok (Some _) -> (ret_to_opt @@ ret), `Help
        | Ok None ->
            match vers_arg with
            | None -> (ret_to_opt @@ ret), ret
            | Some varg ->
                match varg ei cl with
                | Error _ -> None, (`Error `Parse)
                | Ok true -> (ret_to_opt @@ ret), `Version
                | Ok false -> (ret_to_opt @@ ret), ret

  let env_default v = try Some (Sys.getenv v) with Not_found -> None

  let eval
      ?help:(help_ppf = Format.std_formatter)
      ?err:(err_ppf = Format.err_formatter)
      ?(catch = true) ?(env = env_default) ?(argv = Sys.argv) ((al, f), ti) =
    let term = Cmdliner_info.term_add_args ti al in
    let ei = Cmdliner_info.eval ~term ~main:term ~choices:[] ~env in
    eval_term catch help_ppf err_ppf ei f (remove_exec argv)

  let choose_term main choices = function
  | [] -> Ok (main, [])
  | maybe :: args' as args ->
      if String.length maybe > 1 && maybe.[0] = '-' then Ok (main, args) else
      let index =
        let add acc (choice, _ as c) =
          let name = Cmdliner_info.term_name choice in
          match Cmdliner_trie.add acc name c with
          | `New t -> t
          | `Replaced (c', _) -> invalid_arg (err_multi_cmd_def name c c')
        in
        List.fold_left add Cmdliner_trie.empty choices
      in
      match Cmdliner_trie.find index maybe with
      | `Ok choice -> Ok (choice, args')
      | `Not_found ->
          let all = Cmdliner_trie.ambiguities index "" in
          let hints = Cmdliner_suggest.value maybe all in
          Error (Cmdliner_base.err_unknown ~kind:"command" maybe ~hints)
      | `Ambiguous ->
          let ambs = Cmdliner_trie.ambiguities index maybe in
          let ambs = List.sort compare ambs in
          Error (Cmdliner_base.err_ambiguous ~kind:"command" maybe ~ambs)

  let eval_choice
      ?help:(help_ppf = Format.std_formatter)
      ?err:(err_ppf = Format.err_formatter)
      ?(catch = true) ?(env = env_default) ?(argv = Sys.argv)
      main choices =
    let to_term_f ((al, f), ti) = Cmdliner_info.term_add_args ti al, f in
    let choices_f = List.rev_map to_term_f choices in
    let main_f = to_term_f main in
    let choices = List.rev_map fst choices_f in
    let main = fst main_f in
    match choose_term main_f choices_f (remove_exec argv) with
    | Error err ->
        let ei = Cmdliner_info.eval ~term:main ~main ~choices ~env in
        Cmdliner_msg.pp_err_usage err_ppf ei ~err; `Error `Parse
    | Ok ((chosen, f), args) ->
        let ei = Cmdliner_info.eval ~term:chosen ~main ~choices ~env in
        eval_term catch help_ppf err_ppf ei f args

  let eval_peek_opts
      ?(version_opt = false) ?(env = env_default) ?(argv = Sys.argv)
      ((args, f) : 'a t) =
    let version = if version_opt then Some "dummy" else None in
    let term = Cmdliner_info.term ~args ?version "dummy" in
    let ei = Cmdliner_info.eval ~term ~main:term ~choices:[] ~env  in
    (term_eval_peek_opts ei f (remove_exec argv) :> 'a option * 'a result)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
