(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cmdliner programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type info = Cmdliner_info.term
val info :
  ?man_xrefs:Cmdliner_manpage.xref list -> ?man:Cmdliner_manpage.block list ->
  ?envs:Cmdliner_info.env list -> ?exits:Cmdliner_info.exit list ->
  ?sdocs:string -> ?docs:string -> ?doc:string -> ?version:string ->
  string -> info

type 'a t

val v : info -> 'a Cmdliner_term.t -> 'a t
val group : ?default:'a Cmdliner_term.t -> info -> 'a t list -> 'a t

(** {1:eval Eval} *)

type 'a ok = [ `Ok of 'a | `Version | `Help ]
type err = [ `Parse | `Term | `Exn ]

val eval :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array -> 'a t ->
  ('a ok, err) result

val eval_peek_opts :
  ?version_opt:bool ->
  ?env:(string -> string option) ->
  ?argv:string array -> 'a Cmdliner_term.t -> 'a option * ('a ok, err) result

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
