(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A command line stores pre-parsed information about the command
   line's arguments in a more structured way. Given the
   Cmdliner_info.arg values mentioned in a term and Sys.argv
   (without exec name) we parse the command line into a map of
   Cmdliner_info.arg values to [arg] values (see below). This map is used by
   the term's closures to retrieve and convert command line arguments
   (see the Cmdliner_arg module). *)

(* Completion *)

let complete_prefix = "--__complete="
let has_complete_prefix s =
  Cmdliner_base.string_starts_with ~prefix:complete_prefix s

let get_token_to_complete s =
  Cmdliner_base.string_drop_first (String.length complete_prefix) s

let is_opt_to_complete s = (* assert (has_complete_prefix s) *)
  String.length s > String.length complete_prefix &&
  s.[String.length complete_prefix] = '-'

let maybe_token_to_complete ~for_completion s =
  if not for_completion || not (has_complete_prefix s) then None else
  Some (get_token_to_complete s)

exception Completion_requested of Cmdliner_def.Complete.t

let comp_request ?after_dashdash ~prefix kind =
  let comp = Cmdliner_def.Complete.make ?after_dashdash ~prefix kind in
  raise (Completion_requested comp)

(* Command lines *)

let err_multi_opt_name_def name a a' =
  let kind = "option name" in
  Cmdliner_base.err_multi_def ~kind name Cmdliner_def.Arg_info.doc a a'

type arg = Cmdliner_def.Cline.arg
type t = Cmdliner_def.Cline.t

let get_arg cline a : arg =
  try Cmdliner_def.Arg_info.Map.find a cline with Not_found -> assert false

let opt_arg cline a = match get_arg cline a with O l -> l | _ -> assert false
let pos_arg cline a = match get_arg cline a with P l -> l | _ -> assert false
let actual_args cline a = match get_arg cline a with
| P args -> args
| O l ->
    let extract_args (_pos, name, value) =
      name :: (match value with None -> [] | Some v -> [v])
    in
    List.concat (List.map extract_args l)

let arg_info_indexes args =
  (* from [args] returns a trie mapping the names of optional arguments to
     their arg_info, a list with all arg_info for positional arguments and
     a cmdline mapping each arg_info to an empty [arg]. *)
  let rec loop optidx posidx cline = function
  | [] -> optidx, posidx, cline
  | a :: l ->
      match Cmdliner_def.Arg_info.is_pos a with
      | true ->
          let cline = Cmdliner_def.Arg_info.Map.add a (P [] : arg) cline in
          loop optidx (a :: posidx) cline l
      | false ->
          let add t name = match Cmdliner_trie.add t name a with
          | `New t -> t
          | `Replaced (a', _) -> invalid_arg (err_multi_opt_name_def name a a')
          in
          let names = Cmdliner_def.Arg_info.opt_names a in
          let optidx = List.fold_left add optidx names in
          let cline = Cmdliner_def.Arg_info.Map.add a (O [] : arg) cline in
          loop optidx posidx cline l
  in
  let cline = Cmdliner_def.Arg_info.Map.empty in
  loop Cmdliner_trie.empty [] cline (Cmdliner_def.Arg_info.Set.elements args)

(* Optional argument parsing *)

(* Note on option completion. Technically when trying to complete an
   option we could try to avoid mentioning names that have already be
   mentioned and that are not repeatable. Sometimes not being able to
   complete what we know exists ends up being more confusing than
   enlightening so we don't do that for now. *)

let is_opt s = String.length s > 1 && s.[0] = '-'
let is_short_opt s = String.length s = 2 && s.[0] = '-'

let parse_opt_arg s =
  (* (name, value) of opt arg, assert len > 1. except if complete *)
  let is_completion = has_complete_prefix s in
  let s = if is_completion then get_token_to_complete s else s in
  let l = String.length s in
  if l <= 1 then "-", None, is_completion else
  if s.[1] <> '-' then (* short opt *)
    if l = 2 then s, None, is_completion else
    String.sub s 0 2, Some (String.sub s 2 (l - 2)) (* with glued opt arg *),
    is_completion
  else try (* long opt *)
    let i = String.index s '=' in
    String.sub s 0 i, Some (String.sub s (i + 1) (l - i - 1)), is_completion
  with Not_found -> s, None, is_completion

let hint_matching_opt optidx s =
  (* hint options that could match [s] in [optidx]. FIXME explain this is
     a bit obscure. *)
  if String.length s <= 2 then [] else
  let short_opt, long_opt =
    if s.[1] <> '-'
    then s, Printf.sprintf "-%s" s
    else String.sub s 1 (String.length s - 1), s
  in
  let short_opt, _, _ = parse_opt_arg short_opt in
  let long_opt, _, _ = parse_opt_arg long_opt in
  let all = Cmdliner_trie.ambiguities optidx "-" in
  match List.mem short_opt all, Cmdliner_base.suggest long_opt all with
  | false, [] -> []
  | false, l -> l
  | true, [] -> [short_opt]
  | true, l -> if List.mem short_opt l then l else short_opt :: l

let parse_opt_value ~for_completion arg_info name value args =
  (* Either we got a value glued in [value] or we need to get one in args *)
  match Cmdliner_def.Arg_info.opt_kind arg_info with
  | Flag -> (* Flags have no values but we may get dash sharing in [value] *)
      begin match value with
      | None -> value, args
      | Some v when is_short_opt name -> (* short flag dash sharing *)
          None, ("-" ^ v) :: args
      | Some _ -> (* an error but this is reported during typed parsing *)
          value, args
      end
  | _ ->
      match value with
      | Some _ -> value, args
      | None -> (* Get it from the next argument. *)
          match args with
          | [] -> None, args
          | v :: rest when for_completion && has_complete_prefix v ->
              let v = get_token_to_complete v in
              if is_opt v then (* not an option value *) None, args else
              comp_request ~prefix:v (Opt_value arg_info)
          | v :: rest ->
              if is_opt v then None, args else Some v, rest

let try_complete_opt_value arg_info name value args =
  (* At that point we found a matching option name so this should be mostly
     about completing a glued option value, but there are twists. *)
  match Cmdliner_def.Arg_info.opt_kind arg_info with
  | Cmdliner_def.Arg_info.Flag ->
      begin match value with
      | Some v when is_short_opt name ->
          (* short flag dash sharing, push the completion *)
          let args = (complete_prefix ^ "-" ^ v) :: args in
          None, args
      | Some v ->
          (* This is actually a parse error, flags have no value.  We
             make it an option completion but the completions will
             eventually be empty (the prefix won't match) *)
          comp_request ~prefix:(name ^ v) Opt_name
      | None ->
          (* We have in fact a fully completed flag turn it into an
             option completion. *)
          comp_request ~prefix:name Opt_name
      end
  | _ ->
      begin match value with
      | Some prefix -> comp_request ~prefix (Opt_value arg_info)
      | None ->
          (* We have a fully completed option name, we don't try to
             lookup what happens in the next argument which should
             hold the value if any, we just turn it into an option
             completion. *)
          comp_request ~prefix:name Opt_name
      end

let parse_opt_args
    ~peek_opts ~legacy_prefixes ~for_completion optidx cline args
  =
  (* returns an updated [cl] cmdline according to the options found in [args]
     with the trie index [optidx]. Positional arguments are returned in order
     in a list. *)
  let rec loop errs k cline pargs = function
  | [] -> List.rev errs, cline, false, List.rev pargs
  | "--" :: args -> List.rev errs, cline, true, (List.rev_append pargs args)
  | s :: args ->
      let do_parse =
        is_opt s &&
        (if not for_completion then true else
         if not (has_complete_prefix s) then true else
         is_opt_to_complete s)
      in
      if not do_parse then loop errs (k + 1) cline (s :: pargs) args else
      let name, value, is_completion = parse_opt_arg s in
      match Cmdliner_trie.find ~legacy_prefixes optidx name with
      | Ok arg_info ->
          let value, args =
            if is_completion
            then try_complete_opt_value arg_info name value args
            else parse_opt_value ~for_completion arg_info name value args
          in
          let arg : arg = O ((k, name, value) :: opt_arg cline arg_info) in
          let cline = Cmdliner_def.Arg_info.Map.add arg_info arg cline in
          loop errs (k + 1) cline pargs args
      | Error `Not_found when for_completion ->
          if not is_completion
          then (* XXX unclear *) loop errs (k + 1) cline (s :: pargs) args else
          let prefix = name ^ Option.value ~default:"" value in
          comp_request ~prefix Opt_name
      | Error `Not_found when peek_opts ->
          loop errs (k + 1) cline pargs args
      | Error `Not_found ->
          let hints = hint_matching_opt optidx s in
          let err = Cmdliner_base.err_unknown ~kind:"option" ~hints name in
          loop (err :: errs) (k + 1) cline pargs args
      | Error `Ambiguous (* Only on legacy prefixes *) ->
          let ambs = Cmdliner_trie.ambiguities optidx name in
          let ambs = List.sort compare ambs in
          let err = Cmdliner_base.err_ambiguous ~kind:"option" name ~ambs in
          loop (err :: errs) (k + 1) cline pargs args
  in
  let errs, cline, has_dashdash, pargs = loop [] 0 cline [] args in
  if errs = [] then Ok (cline, has_dashdash, pargs) else
  let err = String.concat "\n" errs in
  Error (err, cline, has_dashdash, pargs)

let take_range ~for_completion start stop l =
  let rec loop i acc = function
  | [] -> `Range (List.rev acc)
  | v :: vs ->
      if i < start then loop (i + 1) acc vs else
      if i <= stop then match maybe_token_to_complete ~for_completion v with
      | Some prefix -> `Complete prefix
      | None -> loop (i + 1) (v :: acc) vs
      else `Range (List.rev acc)
  in
  loop 0 [] l

let process_pos_args ~for_completion posidx cline ~has_dashdash pargs =
  (* returns an updated [cl] cmdline in which each positional arg mentioned
     in the list index posidx, is given a value according the list
     of positional arguments values [pargs]. *)
  if pargs = [] then
    let misses = List.filter Cmdliner_def.Arg_info.is_req posidx in
    if misses = [] then Ok cline else
    Error (Cmdliner_msg.err_pos_misses misses, cline)
  else
  let last = List.length pargs - 1 in
  let pos rev k = if rev then last - k else k in
  let rec loop misses cline max_spec = function
  | [] -> misses, cline, max_spec
  | a :: al ->
      let apos = Cmdliner_def.Arg_info.pos_kind a in
      let rev = Cmdliner_def.Arg_info.pos_rev apos in
      let start = pos rev (Cmdliner_def.Arg_info.pos_start apos) in
      let stop = match Cmdliner_def.Arg_info.pos_len apos with
      | None -> pos rev last
      | Some n -> pos rev (Cmdliner_def.Arg_info.pos_start apos + n - 1)
      in
      let start, stop = if rev then stop, start else start, stop in
      let args =
        match take_range ~for_completion start stop pargs with
        | `Range args -> args
        | `Complete prefix ->
            let kind = Cmdliner_def.Complete.Opt_name_or_pos_value a in
            comp_request ~after_dashdash:has_dashdash ~prefix kind
      in
      let max_spec = max stop max_spec in
      let cline = Cmdliner_def.Arg_info.Map.add a (P args : arg) cline in
      let misses = match Cmdliner_def.Arg_info.is_req a && args = [] with
      | true -> a :: misses
      | false -> misses
      in
      loop misses cline max_spec al
  in
  let misses, cline, max_spec = loop [] cline (-1) posidx in
  let consume_excess () =
    match take_range ~for_completion (max_spec + 1) last pargs with
    | `Range args -> args
    | `Complete prefix ->
        comp_request ~after_dashdash:has_dashdash ~prefix Opt_name
  in
  if misses <> [] then begin
    let _ : string list = consume_excess () in
    Error (Cmdliner_msg.err_pos_misses misses, cline)
  end else
  if last <= max_spec then Ok cline else
  Error (Cmdliner_msg.err_pos_excess (consume_excess ()), cline)

let create ?(peek_opts = false) ~legacy_prefixes ~for_completion al args =
  try
    let optidx, posidx, cline = arg_info_indexes al in
    let r =
      parse_opt_args
        ~for_completion ~peek_opts ~legacy_prefixes optidx cline args
    in
    match r with
    | Ok (cline, has_dashdash, _) when peek_opts -> `Ok cline
    | Ok (cline, has_dashdash, pargs) ->
        let r =
          process_pos_args ~for_completion posidx cline ~has_dashdash pargs
        in
        if not for_completion
        then (match r with Ok v -> `Ok v | Error v -> `Error v)
        else begin
          (* Normally [Completion_requested] should have been raised. This
             may fail to happen if pos args are ill defined: we may miss the
             completion token. Just make sure we do a completion. N.B. *)
          match List.find_opt has_complete_prefix pargs with
          | None -> assert false
          | Some arg ->
              match maybe_token_to_complete ~for_completion:true arg with
              | None -> assert false
              | Some prefix ->
                  comp_request ~after_dashdash:has_dashdash ~prefix Opt_name
        end
  | Error (errs, cline, has_dashdash, pargs) ->
      let _ : _ result =
        process_pos_args ~for_completion posidx cline ~has_dashdash pargs
      in
      `Error (errs, cline)
  with Completion_requested c -> `Complete c

(* Deprecations *)

type deprecated = Cmdliner_def.Arg_info.t * arg

let deprecated ~env cline =
  let add ~env info arg acc =
    let deprecation_invoked = match (arg : arg) with
    | O [] | P [] -> (* nothing on the cli for the argument *)
        begin match Cmdliner_def.Arg_info.env info with
        | None -> false
        | Some ienv ->
            (* the parse uses the env var if defined which may be deprecated  *)
            Option.is_some (Cmdliner_def.Env.info_deprecated ienv) &&
            Option.is_some (env (Cmdliner_def.Env.info_var ienv))
        end
    | _ -> Option.is_some (Cmdliner_def.Arg_info.deprecated info)
    in
    if deprecation_invoked then (info, arg) :: acc else acc
  in
  List.rev (Cmdliner_def.Arg_info.Map.fold (add ~env) cline [])

let pp_deprecated ~subst ppf (info, arg) =
  let open Cmdliner_base in
  let plural l = if List.length l > 1 then "s" else "" in
  let subst = Cmdliner_def.Arg_info.doclang_subst ~subst info in
  match (arg : arg) with
  | O [] | P [] ->
      let env = Option.get (Cmdliner_def.Arg_info.env info) in
      let msg = Cmdliner_def.Env.styled_deprecated ~errs:ppf ~subst env in
      Fmt.pf ppf "@[%a @[environment variable %a: %a@]@]"
        Fmt.deprecated () Fmt.code (Cmdliner_def.Env.info_var env)
        Fmt.styled_text msg
  | O os ->
      let plural = plural os in
      let names = List.map (fun (_, n, _) -> n) os in
      let msg = Cmdliner_def.Arg_info.styled_deprecated ~errs:ppf ~subst info in
      Fmt.pf ppf "@[%a @[option%s %a: %a@]@]"
        Fmt.deprecated () plural Fmt.(list ~sep:sp code_or_quote) names
        Fmt.styled_text msg
  | P args ->
      let plural = plural args in
      let msg =
        Cmdliner_def.Arg_info.styled_deprecated ~errs:ppf ~subst info
      in
      Fmt.pf ppf "@[%a @[argument%s %a: %a@]@]"
        Fmt.deprecated () plural Fmt.(list ~sep:sp code_or_quote) args
        Fmt.styled_text msg
