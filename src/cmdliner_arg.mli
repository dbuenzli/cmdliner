(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command line arguments as terms. *)

type 'a parser = string -> ('a, string) result
type 'a printer = Format.formatter -> 'a -> unit
type 'a conv

val conv' :
  ?complete:(string -> (string * string) list) ->
  ?complete_file:bool ->
  ?complete_dir:bool ->
  ?docv:string -> 'a parser * 'a printer -> 'a conv

val conv :
  ?complete:(string -> (string * string) list) ->
  ?complete_file:bool ->
  ?complete_dir:bool ->
  ?docv:string -> (string -> ('a, [`Msg of string]) result) * 'a printer ->
  'a conv

val conv_parser' : 'a conv -> string -> ('a, string) result
val conv_parser : 'a conv -> (string -> ('a, [`Msg of string]) result)
val conv_printer : 'a conv -> 'a printer
val conv_docv : 'a conv -> string

val parser_of_kind_of_string :
  kind:string -> (string -> 'a option) ->
  (string -> ('a, [`Msg of string]) result)

val some : ?none:string -> 'a conv -> 'a option conv
val some' : ?none:'a -> 'a conv -> 'a option conv

type 'a t = 'a Cmdliner_term.t

type info
val info :
  ?deprecated:string -> ?absent:string -> ?docs:string -> ?docv:string ->
  ?doc:string -> ?env:Cmdliner_info.Env.info ->
  string list -> info

val ( & ) : ('a -> 'b) -> 'a -> 'b

val flag : info -> bool t
val flag_all : info -> bool list t
val vflag : 'a -> ('a * info) list -> 'a t
val vflag_all : 'a list -> ('a * info) list -> 'a list t
val opt : ?vopt:'a -> 'a conv -> 'a -> info -> 'a t
val opt_all : ?vopt:'a -> 'a conv -> 'a list -> info -> 'a list t

val pos : ?rev:bool -> int -> 'a conv -> 'a -> info -> 'a t
val pos_all : 'a conv -> 'a list -> info -> 'a list t
val pos_left : ?rev:bool -> int -> 'a conv -> 'a list -> info -> 'a list t
val pos_right : ?rev:bool -> int -> 'a conv -> 'a list -> info -> 'a list t

(** {1 As terms} *)

val value : 'a t -> 'a Cmdliner_term.t
val required : 'a option t -> 'a Cmdliner_term.t
val non_empty : 'a list t -> 'a list Cmdliner_term.t
val last : 'a list t -> 'a Cmdliner_term.t

(** {1 Predefined arguments} *)

val man_format : Cmdliner_manpage.format Cmdliner_term.t
val stdopt_version : docs:string -> bool Cmdliner_term.t
val stdopt_help : docs:string -> Cmdliner_manpage.format option Cmdliner_term.t

(** {1 Converters} *)

val bool : bool conv
val char : char conv
val int : int conv
val nativeint : nativeint conv
val int32 : int32 conv
val int64 : int64 conv
val float : float conv
val string : string conv
val enum : (string * 'a) list -> 'a conv
val file : string conv
val dir : string conv
val non_dir_file : string conv
val list : ?sep:char -> 'a conv -> 'a list conv
val array : ?sep:char -> 'a conv -> 'a array conv
val pair : ?sep:char -> 'a conv -> 'b conv -> ('a * 'b) conv
val t2 : ?sep:char -> 'a conv -> 'b conv -> ('a * 'b) conv
val t3 : ?sep:char -> 'a conv ->'b conv -> 'c conv -> ('a * 'b * 'c) conv
val t4 :
  ?sep:char -> 'a conv ->'b conv -> 'c conv -> 'd conv ->
  ('a * 'b * 'c * 'd) conv

val doc_quote : string -> string
val doc_alts : ?quoted:bool -> string list -> string
val doc_alts_enum : ?quoted:bool -> (string * 'a) list -> string
