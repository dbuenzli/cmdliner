(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

(* Exit codes *)

module Exit = struct
  type code = int

  let ok = 0
  let some_error = 123
  let cli_error = 124
  let internal_error = 125

  type info =
    { codes : code * code; (* min, max *)
      doc : string; (* help. *)
      docs : string; } (* title of help section where listed. *)

  let info
      ?(docs = Cmdliner_manpage.s_exit_status) ?(doc = "undocumented") ?max min
    =
    let max = match max with None -> min | Some max -> max in
    { codes = (min, max); doc; docs }

  let info_codes i = i.codes
  let info_code i = fst i.codes
  let info_doc i = i.doc
  let info_docs i = i.docs
  let info_order i0 i1 = compare i0.codes i1.codes
  let defaults =
    [ info ok ~doc:"on success.";
      info some_error
        ~doc:"on indiscriminate errors reported on standard error.";
      info cli_error ~doc:"on command line parsing errors.";
      info internal_error ~doc:"on unexpected internal errors (bugs)."; ]

  let doclang_subst ~subst i = function
  | "status" -> Some (string_of_int (info_code i))
  | "status_max" -> Some (string_of_int (snd i.codes))
  | id -> subst id
end

(* Environment variables *)

module Env = struct
  type var = string
  type info = (* information about an environment variable. *)
    { id : int; (* unique id for the env var. *)
      deprecated : string option;
      var : string; (* the variable. *)
      doc : string; (* help. *)
      docs : string; } (* title of help section where listed. *)

  let info
      ?deprecated
      ?(docs = Cmdliner_manpage.s_environment) ?(doc = "See option $(opt).") var
    =
    { id = Cmdliner_base.uid (); deprecated; var; doc; docs }

  let info_deprecated i = i.deprecated
  let info_var i = i.var
  let info_doc i = i.doc
  let info_docs i = i.docs
  let info_compare i0 i1 = Int.compare i0.id i1.id

  let doclang_subst ~subst i = function
  | "env" -> Some (strf "$(b,%s)" (Cmdliner_manpage.escape i.var))
  | id -> subst id

  let styled_deprecated ~errs ~subst i = match i.deprecated with
  | None -> "" | Some msg -> Cmdliner_manpage.doc_to_styled ~errs ~subst msg

  let styled_doc ~errs ~subst i =
    Cmdliner_manpage.doc_to_styled ~errs ~subst i.doc

  module Set = Set.Make (struct type t = info let compare = info_compare end)
end

(* Arguments *)

module Arg = struct
  type absence = Err | Val of string Lazy.t | Doc of string
  type opt_kind = Flag | Opt | Opt_vopt of string

  type pos_kind = (* information about a positional argument. *)
    { pos_rev : bool; (* if [true] positions are counted from the end. *)
      pos_start : int; (* start positional argument. *)
      pos_len : int option } (* number of arguments or [None] if unbounded. *)

  let pos ~rev:pos_rev ~start:pos_start ~len:pos_len =
    { pos_rev; pos_start; pos_len}

  let pos_rev p = p.pos_rev
  let pos_start p = p.pos_start
  let pos_len p = p.pos_len

  type t = (* information about a command line argument. *)
    { id : int; (* unique id for the argument. *)
      deprecated : string option; (* deprecation message *)
      absent : absence; (* behaviour if absent. *)
      env : Env.info option; (* environment variable for default value. *)
      doc : string; (* help. *)
      docv : string; (* variable name for the argument in help. *)
      doc_envs : Env.info list; (* environment that needs to be added to docs *)
      docs : string; (* title of help section where listed. *)
      pos : pos_kind; (* positional arg kind. *)
      opt_kind : opt_kind; (* optional arg kind. *)
      opt_names : string list; (* names (for opt args). *)
      opt_all : bool; } (* repeatable (for opt args). *)

  let dumb_pos = pos ~rev:false ~start:(-1) ~len:None

  let make
      ?deprecated ?(absent = "") ?docs ?(doc_envs = []) ?(docv = "")
      ?(doc = "") ?env names
    =
    let dash n = if String.length n = 1 then "-" ^ n else "--" ^ n in
    let opt_names = List.map dash names in
    let docs = match docs with
    | Some s -> s
    | None ->
        match names with
        | [] -> Cmdliner_manpage.s_arguments
        | _ -> Cmdliner_manpage.s_options
    in
    { id = Cmdliner_base.uid (); deprecated; absent = Doc absent;
      env; doc; docv; doc_envs; docs; pos = dumb_pos;
      opt_kind = Flag; opt_names; opt_all = false; }

  let id i = i.id
  let deprecated i = i.deprecated
  let absent i = i.absent
  let env i = i.env
  let doc i = i.doc
  let docv i = i.docv
  let doc_envs i = i.doc_envs
  let docs i = i.docs
  let pos_kind i = i.pos
  let opt_kind i = i.opt_kind
  let opt_names i = i.opt_names
  let opt_all i = i.opt_all
  let opt_name_sample i =
    (* First long or short name (in that order) in the list; this
       allows the client to control which name is shown *)
    let rec find = function
    | [] -> List.hd i.opt_names
    | n :: ns -> if (String.length n) > 2 then n else find ns
    in
    find i.opt_names

  let make_req i = { i with absent = Err }
  let make_all_opts i = { i with opt_all = true }
  let make_opt ~docv ~absent ~kind:opt_kind i =
    { i with absent; opt_kind; docv }

  let make_opt_all ~docv ~absent ~kind:opt_kind i =
    { i with absent; opt_kind; opt_all = true; docv  }

  let make_pos ~docv ~pos i = { i with pos; docv }
  let make_pos_abs ~docv ~absent ~pos i = { i with absent; pos; docv }

  let is_opt i = i.opt_names <> []
  let is_pos i = i.opt_names = []
  let is_req i = i.absent = Err

  let pos_cli_order a0 a1 = (* best-effort order on the cli. *)
    let c = compare (a0.pos.pos_rev) (a1.pos.pos_rev) in
    if c <> 0 then c else
    if a0.pos.pos_rev
    then compare a1.pos.pos_start a0.pos.pos_start
    else compare a0.pos.pos_start a1.pos.pos_start

  let rev_pos_cli_order a0 a1 = pos_cli_order a1 a0

  let compare a0 a1 = Int.compare a0.id a1.id

  let doclang_subst ~subst i = function
  | "docv" -> Some (strf "$(i,%s)" (Cmdliner_manpage.escape i.docv))
  | "opt" when is_opt i ->
      Some (strf "$(b,%s)" (Cmdliner_manpage.escape (opt_name_sample i)))
  | id ->
      match env i with
      | Some e -> Env.doclang_subst ~subst e id
      | None -> subst id

  let styled_deprecated ~errs ~subst i = match i.deprecated with
  | None -> "" | Some msg -> Cmdliner_manpage.doc_to_styled ~errs ~subst msg

  let styled_doc ~errs ~subst i =
    Cmdliner_manpage.doc_to_styled ~errs ~subst i.doc

  module Set = struct
    type arg = t
    type completion =
    | V : 'a Cmdliner_base.Completion.t -> completion

    module Map = Map.Make (struct type t = arg let compare = compare end)
    include Map

    type t = completion Map.t

    let find_opt k m = try Some (Map.find k m) with Not_found -> None
    let elements m = List.map fst (bindings m)
    let union a b =
      Map.merge (fun k v v' ->
        match v, v' with
        | Some v, _ | _, Some v -> Some v
        | None, None -> assert false) a b
  end
end

(* Commands *)

module Cmd = struct
  type t =
    { name : string; (* name of the cmd. *)
      version : string option; (* version (for --version). *)
      deprecated : string option; (* deprecation message *)
      doc : string; (* one line description of cmd. *)
      docs : string; (* title of man section where listed (commands). *)
      sdocs : string; (* standard options, title of section where listed. *)
      exits : Exit.info list; (* exit codes for the cmd. *)
      envs : Env.info list; (* env vars that influence the cmd. *)
      man : Cmdliner_manpage.block list; (* man page text. *)
      man_xrefs : Cmdliner_manpage.xref list; (* man cross-refs. *)
      args : Arg.Set.t; (* Command arguments. *)
      has_args : bool; (* [true] if has own parsing term. *)
      children : t list; } (* Children, if any. *)

  let make
      ?deprecated ?(man_xrefs = [`Main]) ?(man = []) ?(envs = [])
      ?(exits = Exit.defaults) ?(sdocs = Cmdliner_manpage.s_common_options)
      ?(docs = Cmdliner_manpage.s_commands) ?(doc = "") ?version name
    =
    { name; version; deprecated; doc; docs; sdocs; exits;
      envs; man; man_xrefs; args = Arg.Set.empty;
      has_args = true; children = [] }

  let name i = i.name
  let version i = i.version
  let deprecated i = i.deprecated
  let doc i = i.doc
  let docs i = i.docs
  let stdopts_docs i = i.sdocs
  let exits i = i.exits
  let envs i = i.envs
  let man i = i.man
  let man_xrefs i = i.man_xrefs
  let args i = i.args
  let has_args i = i.has_args
  let children i = i.children
  let add_args i args = { i with args = Arg.Set.union args i.args }
  let with_children cmd ~args ~children =
    let has_args, args = match args with
    | None -> false, cmd.args
    | Some args -> true, Arg.Set.union args cmd.args
    in
    { cmd with has_args; args; children }

  let styled_deprecated ~errs ~subst i = match i.deprecated with
  | None -> "" | Some msg -> Cmdliner_manpage.doc_to_styled ~errs ~subst msg

  let styled_doc ~errs ~subst i =
    Cmdliner_manpage.doc_to_styled ~errs ~subst i.doc

  let escaped_name i = Cmdliner_manpage.escape i.name
end

(* Evaluation *)

module Eval = struct
  type t = (* information about the evaluation context. *)
    { cmd : Cmd.t; (* cmd being evaluated. *)
      parents : Cmd.t list; (* parents of cmd, root is last. *)
      env : string -> string option; (* environment variable lookup. *)
      err_ppf : Format.formatter (* error formatter *) }

  let make ~cmd ~parents ~env ~err_ppf = { cmd; parents; env; err_ppf }

  let cmd i = i.cmd
  let parents i = i.parents
  let env_var i v = i.env v
  let err_ppf i = i.err_ppf
  let main i = match List.rev i.parents with [] -> i.cmd | m :: _ -> m
  let with_cmd i cmd = { i with cmd }

  let doclang_name n = strf "$(b,%s)" (Cmd.escaped_name n)
  let doclang_names names =
    strf "$(b,%s)" (Cmdliner_manpage.escape (String.concat " " names))

  let doclang_subst ei = function
  | "tname" | "cmd.name" -> Some (doclang_name ei.cmd)
  | "mname" | "tool" -> Some (doclang_name (main ei))
  | "cmd.parent" ->
      let parents = parents ei in
      if parents = [] then Some (doclang_name (main ei)) else
      Some (doclang_names (List.rev_map Cmd.name parents))
  | "iname" | "cmd" ->
      Some (doclang_names (List.rev_map Cmd.name (cmd ei :: parents ei)))
  | _ -> None
end
