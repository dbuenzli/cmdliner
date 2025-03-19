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
  val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  val indent : int t
  val text : string t
  val lines : string t
  val tokens : spaces:bool -> string t
end

(* Error message helpers *)

val quote : string -> string
val alts_str : ?quoted:bool -> string list -> string
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
end

val some : ?none:string -> 'a Conv.t -> 'a option Conv.t
val some' : ?none:'a -> 'a Conv.t -> 'a option Conv.t
val bool : bool Conv.t
val char : char Conv.t
val int : int Conv.t
val nativeint : nativeint Conv.t
val int32 : int32 Conv.t
val int64 : int64 Conv.t
val float : float Conv.t
val string : string Conv.t
val enum : (string * 'a) list -> 'a Conv.t
val file : string Conv.t
val dir : string Conv.t
val non_dir_file : string Conv.t
val list : ?sep:char -> 'a Conv.t -> 'a list Conv.t
val array : ?sep:char -> 'a Conv.t -> 'a array Conv.t
val pair : ?sep:char -> 'a Conv.t -> 'b Conv.t -> ('a * 'b) Conv.t
val t2 : ?sep:char -> 'a Conv.t -> 'b Conv.t -> ('a * 'b) Conv.t
val t3 :
  ?sep:char -> 'a Conv.t ->'b Conv.t -> 'c Conv.t -> ('a * 'b * 'c) Conv.t

val t4 :
  ?sep:char -> 'a Conv.t -> 'b Conv.t -> 'c Conv.t -> 'd Conv.t ->
  ('a * 'b * 'c * 'd) Conv.t

val env_bool_parse : bool Conv.parser
