(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A few helpful base definitions. *)

val uid : unit -> int
(** [uid ()] is new unique for the program run. *)

val suggest : string -> string list -> string list
(** [suggest near candidates]  suggest values from [candidates]
    not too far from [near]. *)

val is_space : char -> bool
val string_has_prefix : prefix:string -> string -> bool
val string_drop_prefix : prefix:string -> string -> string option

(* Formatters *)

module Fmt : sig
  type 'a t = Format.formatter -> 'a -> unit
  val str : ('a, Format.formatter, unit, string) format4 -> 'a
  val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  val sp : unit t
  val comma : unit t
  val cut : unit t
  val char : char t
  val string : string t
  val indent : int t
  val list : ?sep:unit t -> 'a t -> 'a list t
  val styled_text : string t
  val lines : string t
  val tokens : spaces:bool -> string t
  val code : string t
  val code_var : string t
  val code_or_quote : string t
  val ereason : string t
  val missing : unit t
  val invalid : unit t
  val puterr : unit t

  type styler = Ansi | Plain
  val styler : unit -> styler
end

(* Error message helpers *)

val quote : string -> string
val pp_alts : string list Fmt.t
val alts_str : ?quoted:bool -> string list -> string
val err_empty_list : string
val err_ambiguous : kind:string -> string -> ambs:string list -> string
val err_unknown :
  ?dom:string list -> ?hints:string list -> kind:string -> string -> string
val err_multi_def :
  kind:string -> string -> ('b -> string) -> 'b -> 'b -> string

(* Completion strategies *)

module Completion : sig
  type complete = string -> (string * string) list
  type 'a t
  val make : ?files:bool -> ?dirs:bool -> ?complete:complete -> unit -> 'a t
  val none : 'a t
  val files : 'a t -> bool
  val dirs : 'a t -> bool
  val complete : 'a t -> complete
  val some : 'a t -> 'a option t
end

(* Textual OCaml value converters *)

module Conv : sig
  type 'a parser = string -> ('a, string) result
  type 'a fmt = 'a Fmt.t
  type 'a t
  val make :
    ?completion:'a Completion.t -> docv:string -> parser:'a parser ->
    pp:'a fmt -> unit -> 'a t

  val of_conv : 'a t ->
    ?completion:'a Completion.t -> ?docv:string -> ?parser:'a parser ->
    ?pp:'a fmt -> unit -> 'a t

  val docv : 'a t -> string
  val parser : 'a t -> 'a parser
  val pp : 'a t -> 'a fmt
  val completion : 'a t -> 'a Completion.t

  val some : ?none:string -> 'a t -> 'a option t
  val some' : ?none:'a -> 'a t -> 'a option t
end
