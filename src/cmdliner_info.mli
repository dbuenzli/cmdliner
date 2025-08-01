(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Exit codes, environment variables, arguments, commands and eval information.

    These information types gathers untyped data used to parse command
    lines report errors and format man pages. *)

(** Exit codes. *)
module Exit : sig
  type code = int
  val ok : code
  val some_error : code
  val cli_error : code
  val internal_error : code

  type info
  val info : ?docs:string -> ?doc:string -> ?max:code -> code -> info
  val info_code : info -> code
  val info_codes : info -> code * code
  val info_doc : info -> string
  val info_docs : info  -> string
  val info_order : info -> info -> int
  val defaults : info list
  val doclang_subst :
    subst:Cmdliner_manpage.subst -> info -> Cmdliner_manpage.subst
  (** [doclang_subst ~subst info] adds the substitution of [info] to
      [subst]. *)
end

(** Environment variables. *)
module Env : sig
  type var = string
  type info
  val info : ?deprecated:string -> ?docs:string -> ?doc:string -> var -> info
  val info_var : info -> string
  val info_doc : info -> string
  val info_docs : info -> string
  val info_deprecated : info -> string option
  val doclang_subst :
    subst:Cmdliner_manpage.subst -> info -> Cmdliner_manpage.subst
  (** [doclang_subst ~subst info] adds the substitution of [info] to
      [subst]. *)

  val styled_deprecated :
    errs:Format.formatter -> subst:Cmdliner_manpage.subst -> info -> string

  val styled_doc :
    errs:Format.formatter -> subst:Cmdliner_manpage.subst -> info -> string

  module Set : Set.S with type elt = info
end

(** Arguments *)
module Arg : sig

  type absence =
  | Err  (** an error is reported. *)
  | Val of string Lazy.t (** if <> "", takes the given default value. *)
  | Doc of string
    (** if <> "", a doc string interpreted in the doc markup language. *)
  (** The type for what happens if the argument is absent from the cli. *)

  type opt_kind =
  | Flag (** without value, just a flag. *)
  | Opt  (** with required value. *)
  | Opt_vopt of string (** with optional value, takes given default. *)
  (** The type for optional argument kinds. *)

  type pos_kind
  val pos : rev:bool -> start:int -> len:int option -> pos_kind
  val pos_rev : pos_kind -> bool
  val pos_start : pos_kind -> int
  val pos_len : pos_kind -> int option

  type t
  val make :
    ?deprecated:string -> ?absent:string -> ?docs:string ->
    ?doc_envs:Env.info list -> ?docv:string -> ?doc:string ->
    ?env:Env.info -> string list -> t

  val id : t -> int
  val deprecated : t -> string option
  val absent : t -> absence
  val env : t -> Env.info option
  val doc : t -> string
  val docv : t -> string
  val doc_envs : t -> Env.info list
  val docs : t -> string
  val opt_names : t -> string list (* has dashes *)
  val opt_name_sample : t -> string (* warning must be an opt arg *)
  val opt_kind : t -> opt_kind
  val pos_kind : t -> pos_kind

  val make_req : t -> t
  val make_all_opts : t -> t
  val make_opt : docv:string -> absent:absence -> kind:opt_kind -> t -> t
  val make_opt_all : docv:string -> absent:absence -> kind:opt_kind -> t -> t
  val make_pos : docv:string -> pos:pos_kind -> t -> t
  val make_pos_abs : docv:string -> absent:absence -> pos:pos_kind -> t -> t

  val is_opt : t -> bool
  val is_pos : t -> bool
  val is_req : t -> bool

  val pos_cli_order : t -> t -> int
  val rev_pos_cli_order : t -> t -> int

  val compare : t -> t -> int

  val doclang_subst :
    subst:Cmdliner_manpage.subst -> t -> Cmdliner_manpage.subst
  (** [doclang_subst ~subst info] adds the substitution of [info] to
      [subst]. Note this includes the substitutions for [env] if present. *)

  val styled_deprecated :
    errs:Format.formatter -> subst:Cmdliner_manpage.subst -> t -> string

  val styled_doc :
    errs:Format.formatter -> subst:Cmdliner_manpage.subst -> t -> string

  module Set : sig
    type arg = t
    type completion =
    | V : 'a Cmdliner_base.Completion.t -> completion

    type t
    val is_empty : t -> bool
    val empty : t
    val add : arg -> completion -> t -> t
    val choose : t -> arg * completion
    val partition : (arg -> completion -> bool) -> t -> t * t
    val filter : (arg -> completion -> bool) -> t -> t
    val iter : (arg -> completion -> unit) -> t -> unit
    val singleton : arg -> completion -> t
    val fold : (arg -> completion -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val elements : t -> arg list
    val union : t -> t -> t
    val find_opt : arg -> t -> completion option
  end
end

(** Commands. *)
module Cmd : sig
  type t
  val make :
    ?deprecated:string -> ?man_xrefs:Cmdliner_manpage.xref list ->
    ?man:Cmdliner_manpage.block list -> ?envs:Env.info list ->
    ?exits:Exit.info list -> ?sdocs:string -> ?docs:string -> ?doc:string ->
    ?version:string -> string -> t

  val name : t -> string
  val version : t -> string option
  val deprecated : t -> string option
  val doc : t -> string
  val docs : t -> string
  val stdopts_docs : t -> string
  val exits : t -> Exit.info list
  val envs : t -> Env.info list
  val man : t -> Cmdliner_manpage.block list
  val man_xrefs : t -> Cmdliner_manpage.xref list
  val args : t -> Arg.Set.t
  val has_args : t -> bool
  val children : t -> t list
  val add_args : t -> Arg.Set.t -> t
  val with_children : t -> args:Arg.Set.t option -> children:t list -> t
  val styled_deprecated :
    errs:Format.formatter -> subst:Cmdliner_manpage.subst -> t -> string

  val styled_doc :
    errs:Format.formatter -> subst:Cmdliner_manpage.subst -> t -> string
end

(** Evaluation. *)
module Eval : sig
  type t
  val make :
    cmd:Cmd.t -> ancestors:Cmd.t list -> env:(string -> string option) ->
    err_ppf:Format.formatter -> t

  val cmd : t -> Cmd.t
  val main : t -> Cmd.t
  val ancestors : t -> Cmd.t list (* root is last *)
  val env_var : t -> string -> string option
  val err_ppf : t -> Format.formatter
  val with_cmd : t -> Cmd.t -> t
  val doclang_subst : t -> Cmdliner_manpage.subst
end

(** Completion *)
module Completion : sig
  type kind =
  | Opt_value of Arg.t
  | Opt_name_or_pos_value of Arg.t
  | Opt_name

  type t =
    { prefix : string;
      after_dashdash : bool;
      subcmds : bool; (* Note this is adjusted in Cmdliner_eval *)
      kind : kind  }

  val make :
    ?after_dashdash:bool -> ?subcmds:bool -> prefix:string -> kind -> t

  val add_subcmds : t -> t
end
