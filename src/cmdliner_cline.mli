(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command lines. *)

val is_opt : string -> bool

(** {1 Completion} *)

val has_complete_prefix : string -> bool

type completion_kind =
  [ `Opt of Cmdliner_info.Arg.t
  | `Arg of Cmdliner_info.Arg.t
  | `Any ]

type completion =
  { prefix : string;
    after_dashdash : bool;
    subcmds : bool; (* Note this is adjusted in Cmdliner_eval *)
    kind : completion_kind  }

(** {1: Completion kind} *)

type t

val create :
  ?peek_opts:bool -> legacy_prefixes:bool -> for_completion:bool ->
  Cmdliner_info.Arg.Set.t -> string list ->
  [ `Ok of t | `Completion of completion | `Error of string * t ]

val opt_arg : t -> Cmdliner_info.Arg.t -> (int * string * (string option)) list
val pos_arg : t -> Cmdliner_info.Arg.t -> string list
val actual_args : t -> Cmdliner_info.Arg.t -> string list
(** Actual command line arguments from the command line *)

(** {1:deprecations Deprecations} *)

type deprecated
(** The type for deprecation invocations. This include both environment
    variable deprecations and argument deprecations. *)

val deprecated : env:(string -> string option) -> t -> deprecated list
(** [deprecated ~env cli] are the deprecated invocations that occur
    when parsing [cli]. *)

val pp_deprecated :
  subst:Cmdliner_manpage.subst -> deprecated Cmdliner_base.Fmt.t
(** [pp_deprecated] formats deprecations. *)
