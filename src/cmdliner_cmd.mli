(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cmdliner programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Exit : sig
  type code = int
  val ok : code
  val some_error : code
  val cli_error : code
  val internal_error : code

  type info = Cmdliner_info.exit
  val info : ?docs:string -> ?doc:string -> ?max:code -> code -> info
  val info_code : info -> code
  val defaults : info list
end

module Env : sig
  type var = string
  type info = Cmdliner_info.env
  val info : ?docs:string -> ?doc:string -> var -> info
end

(** {1:cmds Commands} *)

type info = Cmdliner_info.term

val info :
  ?man_xrefs:Cmdliner_manpage.xref list -> ?man:Cmdliner_manpage.block list ->
  ?envs:Cmdliner_info.env list -> ?exits:Cmdliner_info.exit list ->
  ?sdocs:string -> ?docs:string -> ?doc:string -> ?version:string ->
  string -> info

val info_name : info -> string

type 'a t
val v : info -> 'a Cmdliner_term.t -> 'a t
val group : ?default:'a Cmdliner_term.t -> info -> 'a t list -> 'a t

val get_info : 'a t -> info

(** {1:eval Evaluating commands} *)

type 'a eval_ok = [ `Ok of 'a | `Version | `Help ]
type eval_error = [ `Parse | `Term | `Exn ]

val eval_value :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array -> 'a t ->
  ('a eval_ok, eval_error) result

val eval_peek_opts :
  ?version_opt:bool -> ?env:(string -> string option) ->
  ?argv:string array -> 'a Cmdliner_term.t ->
  'a option * ('a eval_ok, eval_error) result

val eval :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array ->
  ?term_err:int -> unit t -> int

val eval' :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array ->
  ?term_err:int -> int t -> int

val eval_result :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array ->
  ?term_err:Exit.code -> (unit, string) result t -> Exit.code

val eval_result' :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array ->
  ?term_err:Exit.code -> (Exit.code, string) result t -> Exit.code

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cmdliner programmers

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
