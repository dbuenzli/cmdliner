(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Manpages.

    See {!Cmdliner.Manpage}. *)

type block =
  [ `S of string | `P of string | `Pre of string | `I of string * string
  | `Noblank ]

type title = string * int * string * string * string
type t = title * block list

(* Standard section names *)

val s_name : string
val s_synopsis : string
val s_description : string
val s_commands : string
val s_arguments : string
val s_options : string
val s_environment : string
val s_files : string
val s_exit_status : string
val s_examples : string
val s_bugs : string
val s_author : string
val s_see_also : string

(* Output *)

type format = [ `Auto | `Pager | `Plain | `Groff ]
val print : ?subst:(string -> string) -> format -> Format.formatter -> t -> unit

(* Printers and escapes *)

val pp_text : Format.formatter -> string -> unit
val pp_lines : Format.formatter -> string -> unit

val plain_esc : char -> string -> string
val escape :
  (string -> string) -> (char -> string -> string) -> Buffer.t -> string ->
  string

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
