(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command lines. *)

type t

val create :
  ?peek_opts:bool -> legacy_prefixes:bool -> Cmdliner_info.Arg.Set.t ->
  string list ->
  [ `Ok of t
  | `Completion of
      string *
      [ `Opt of Cmdliner_info.Arg.t | `Arg of Cmdliner_info.Arg.t | `Any ]
  | `Error of string * t ]

val opt_arg : t -> Cmdliner_info.Arg.t -> (int * string * (string option)) list
val pos_arg : t -> Cmdliner_info.Arg.t -> string list
val actual_args : t -> Cmdliner_info.Arg.t -> string list
(** Actual command line arguments from the command line *)

val is_opt : string -> bool
val deprecated_msgs : t -> string list
