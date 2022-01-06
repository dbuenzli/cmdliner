(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Manpage = Cmdliner_manpage
module Arg = Cmdliner_arg
module Term = struct
  type ('a, 'b) stdlib_result = ('a, 'b) result

  include Cmdliner_term

  (* Deprecated *)

  let man_format = Cmdliner_arg.man_format
  let pure = const

  (* Term information *)

  type exit_info = Cmdliner_info.exit
  let exit_info = Cmdliner_info.exit

  let exit_status_success = 0
  let exit_status_cli_error = 124
  let exit_status_internal_error = 125
  let default_error_exits =
    [ exit_info exit_status_cli_error ~doc:"on command line parsing errors.";
      exit_info exit_status_internal_error
        ~doc:"on unexpected internal errors (bugs)."; ]

  let default_exits =
    (exit_info exit_status_success ~doc:"on success.") :: default_error_exits

  type env_info = Cmdliner_info.env
  let env_info = Cmdliner_info.env

  type info = Cmdliner_info.term
  let info = Cmdliner_info.term ~args:Cmdliner_info.Args.empty
  let name ti = Cmdliner_info.term_name ti

  (* Evaluation *)

  type 'a result =
  [ `Ok of 'a | `Error of [`Parse | `Term | `Exn ] | `Version | `Help ]

  let to_legacy_result = function
  | Ok (#Cmdliner_cmd.ok as r) -> (r : 'a result)
  | Error e -> `Error e

  let eval ?help ?err ?catch ?env ?argv (t, i) =
    let cmd = Cmdliner_cmd.v i t in
    to_legacy_result (Cmdliner_cmd.eval ?help ?err ?catch ?env ?argv cmd)

  let eval_choice ?help ?err ?catch ?env ?argv (t, i) choices =
    let sub (t, i) = Cmdliner_cmd.v i t in
    let cmd = Cmdliner_cmd.group i ~default:t (List.map sub choices) in
    to_legacy_result (Cmdliner_cmd.eval ?help ?err ?catch ?env ?argv cmd)

  let eval_peek_opts ?version_opt ?env ?argv t =
    let o, r = Cmdliner_cmd.eval_peek_opts ?version_opt ?env ?argv t in
    o, to_legacy_result r

  (* Exits *)

  let exit_status_of_result ?(term_err = 1) = function
  | `Ok () | `Help | `Version -> exit_status_success
  | `Error `Term -> term_err
  | `Error `Exn -> exit_status_internal_error
  | `Error `Parse -> exit_status_cli_error

  let exit_status_of_status_result ?term_err = function
  | `Ok n -> n
  | `Help | `Version | `Error _ as r -> exit_status_of_result ?term_err r

  let stdlib_exit = exit
  let exit ?term_err r = stdlib_exit (exit_status_of_result ?term_err r)
  let exit_status ?term_err r =
    stdlib_exit (exit_status_of_status_result ?term_err r)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers

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
