(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

module Manpage = Cmdliner_manpage

(* Invalid_arg strings *)

let err_argv = "argv array must have at least one element"
let err_not_opt = "Option argument without name"
let err_not_pos = "Positional argument with a name"
let err_help s = "Term error, help requested for unknown command " ^ s
let err_empty_list = "Empty list"
let err_incomplete_enum = "Incomplete enumeration for the type"

(* A few useful definitions. *)

let rev_compare n n' = compare n' n
let pr = Format.fprintf
let pp_str = Format.pp_print_string
let pp_char = Format.pp_print_char
let pp_text = Manpage.pp_text
let pp_lines = Manpage.pp_lines
let str_of_pp pp v = pp Format.str_formatter v; Format.flush_str_formatter ()

let quote s = strf "`%s'" s
let alts_str ?(quoted = true) alts =
  let quote = if quoted then quote else (fun s -> s) in
  match alts with
  | [] -> invalid_arg err_empty_list
  | [a] -> (quote a)
  | [a; b] -> strf "either %s or %s" (quote a) (quote b)
  | alts ->
      let rev_alts = List.rev alts in
      strf "one of %s or %s"
        (String.concat ", " (List.rev_map quote (List.tl rev_alts)))
        (quote (List.hd rev_alts))

(* Levenshtein distance, for making spelling suggestions in case of error. *)

let levenshtein_distance s t =
  (* As found here http://rosettacode.org/wiki/Levenshtein_distance#OCaml *)
  let minimum a b c = min a (min b c) in
  let m = String.length s in
  let n = String.length t in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between
     the first i characters of s and the first j characters of t *)
  let d = Array.make_matrix (m+1) (n+1) 0 in
  for i = 0 to m do d.(i).(0) <- i done;
  for j = 0 to n do d.(0).(j) <- j done;
  for j = 1 to n do
    for i = 1 to m do
      if s.[i-1] = t.[j-1] then
        d.(i).(j) <- d.(i-1).(j-1)  (* no operation required *)
      else
        d.(i).(j) <- minimum
            (d.(i-1).(j) + 1)   (* a deletion *)
            (d.(i).(j-1) + 1)   (* an insertion *)
            (d.(i-1).(j-1) + 1) (* a substitution *)
    done;
  done;
  d.(m).(n)

let suggest s candidates =
  let add (min, acc) name =
    let d = levenshtein_distance s name in
    if d = min then min, (name :: acc) else
    if d < min then d, [name] else
    min, acc
  in
  let dist, suggs = List.fold_left add (max_int, []) candidates in
  if dist < 3 (* suggest only if not too far *) then suggs else []

(* The following types keep untyped information about arguments and
   terms. This data is used to parse the command line, report errors
   and format man page information. *)

type env_info =                (* information about an environment variable. *)
  { env_var : string;                                       (* the variable. *)
    env_doc : string;                                               (* help. *)
    env_docs : string; }              (* title of help section where listed. *)

type absence =        (* what happens if the argument is absent from the cl. *)
  | Error                                           (* an error is reported. *)
  | Val of string Lazy.t         (* if <> "", takes the given default value. *)

type opt_kind =                              (* kinds of optional arguments. *)
  | Flag                                      (* just a flag, without value. *)
  | Opt                                                (* value is required. *)
  | Opt_vopt of string     (* option value is optional, takes given default. *)

type pos_kind =                  (* type for ranges of positional arguments. *)
  { pos_rev : bool;         (* if [true] positions are counted from the end. *)
    pos_start : int;                           (* start positional argument. *)
    pos_len : int option }    (* number of arguments or [None] if unbounded. *)

type arg_info =                (* information about a command line argument. *)
  { id : int;                               (* unique id for the argument. *)
    absent : absence;                              (* behaviour if absent. *)
    env_info : env_info option;                   (* environment variable. *)
    doc : string;                                                 (* help. *)
    docv : string;              (* variable name for the argument in help. *)
    docs : string;                  (* title of help section where listed. *)
    p_kind : pos_kind;                             (* positional arg kind. *)
    o_kind : opt_kind;                               (* optional arg kind. *)
    o_names : string list;                        (* names (for opt args). *)
    o_all : bool; }                          (* repeatable (for opt args). *)

let arg_id =        (* thread-safe UIDs, Oo.id (object end) was used before. *)
  let c = ref 0 in
  fun () ->
    let id = !c in
    incr c; if id > !c then assert false (* too many ids *) else id

let is_opt a = a.o_names <> []
let is_pos a = a.o_names = []

module Amap = Map.Make                                     (* arg info maps. *)
    (struct type t = arg_info let compare a a' = compare a.id a'.id end)

type arg =        (* unconverted argument data as found on the command line. *)
  | O of (int * string * (string option)) list (* (pos, name, value) of opt. *)
  | P of string list

type cmdline = arg Amap.t      (* command line, maps arg_infos to arg value. *)

type term_info =
  { name : string;                                    (* name of the term. *)
    version : string option;                   (* version (for --version). *)
    tdoc : string;                        (* one line description of term. *)
    tdocs : string;       (* title of man section where listed (commands). *)
    sdocs : string;    (* standard options, title of section where listed. *)
    man : Manpage.block list; }                          (* man page text. *)

type eval_info =                (* information about the evaluation context. *)
  { term : term_info * arg_info list;               (* term being evaluated. *)
    main : term_info * arg_info list;                          (* main term. *)
    choices : (term_info * arg_info list) list;         (* all term choices. *)
    env : string -> string option }          (* environment variable lookup. *)

let eval_kind ei =                       (* evaluation with multiple terms ? *)
  if ei.choices = [] then `Simple else
  if (fst ei.term) == (fst ei.main) then `M_main else `M_choice

module Help = struct

  let esc = Manpage.markup_text_escape

  let sorted_items_to_blocks ~boilerplate:b items =
    (* Items are sorted by section and then rev. sorted by appearance.
       We gather them by section in correct order in a `Block and prefix
       them with optional boilerplate *)
    let boilerplate = match b with None -> (fun _ -> None) | Some b -> b in
    let mk_block sec acc = match boilerplate sec with
    | None -> (sec, `Blocks acc)
    | Some b -> (sec, `Blocks (b :: acc))
    in
    let rec loop secs sec acc = function
    | (sec', it) :: its when sec' = sec -> loop secs sec (it :: acc) its
    | (sec', it) :: its -> loop (mk_block sec acc :: secs) sec' [it] its
    | [] -> (mk_block sec acc) :: secs
    in
    match items with
    | [] -> []
    | (sec, it) :: its -> loop [] sec [it] its

  (* Doc string variables substitutions. *)

  let term_info_subst ei = function
  | "tname" -> Some (strf "$(b,%s)" @@ esc (fst ei.term).name)
  | "mname" -> Some (strf "$(b,%s)" @@ esc (fst ei.main).name)
  | _ -> None

  let arg_info_subst ~subst a = function
  | "docv" -> Some (strf "$(i,%s)" @@ esc a.docv)
  | "opt" when is_opt a ->
      let k = String.lowercase (List.hd (List.sort compare a.o_names)) in
      Some (strf "$(b,%s)" @@ esc k)
  | "env" when a.env_info <> None ->
      begin match a.env_info with
      | None -> assert false
      | Some v -> Some (strf "$(b,%s)" @@ esc v.env_var)
      end
  | id -> subst id

  (* Command docs *)

  let invocation ?(sep = ' ') ei = match eval_kind ei with
  | `Simple | `M_main -> (fst ei.main).name
  | `M_choice -> strf "%s%c%s" (fst ei.main).name sep (fst ei.term).name

  let synopsis_pos_arg a =
    let v = if a.docv = "" then "$(i,ARG)" else strf "$(i,%s)" (esc a.docv) in
    let v = if a.absent = Error then strf "%s" v else strf "[%s]" v in
    match a.p_kind.pos_len with
    | None -> v ^ "..."
    | Some 1 -> v
    | Some n ->
        let rec loop n acc = if n <= 0 then acc else loop (n - 1) (v :: acc) in
        String.concat " " (loop n [])

  let rev_pos_arg_cli_order (p0, _) (p1, _) =
    (* best-effort reverse order on the cli *)
    let c = compare p0.pos_rev p1.pos_rev in
    if c <> 0 then c else
    if p0.pos_rev
    then compare p0.pos_start p1.pos_start
    else compare p1.pos_start p0.pos_start

  let synopsis ei = match eval_kind ei with
  | `M_main -> strf "$(b,%s) $(i,COMMAND) ..." @@ esc (invocation ei)
  | `Simple | `M_choice ->
      let add_pos_arg acc arg =
        if is_opt arg then acc else (arg.p_kind, synopsis_pos_arg arg) :: acc
      in
      let pargs = List.fold_left add_pos_arg [] (snd ei.term) in
      let pargs = List.sort rev_pos_arg_cli_order pargs in
      let pargs = String.concat " " (List.rev_map snd pargs) in
      strf "$(b,%s) [$(i,OPTION)]... %s" (esc @@ invocation ei) pargs

  let cmd_man_docs ei = match eval_kind ei with
  | `Simple | `M_choice -> []
  | `M_main ->
      let add_cmd acc (ti, _) =
        let cmd = strf "$(b,%s)" @@ esc ti.name in
        (ti.tdocs, `I (cmd, ti.tdoc)) :: acc
      in
      let by_sec_by_rev_name (s0, `I (c0, _)) (s1, `I (c1, _)) =
        let c = compare s0 s1 in
        if c <> 0 then c else compare c1 c0 (* N.B. reverse *)
      in
      let cmds = List.fold_left add_cmd [] ei.choices in
      let cmds = List.sort by_sec_by_rev_name cmds in
      let cmds = (cmds :> (string * Manpage.block) list) in
      sorted_items_to_blocks ~boilerplate:None cmds

  (* Argument docs *)

  let arg_man_item_label a =
    if is_pos a then strf "$(i,%s)" (esc a.docv) else
    let fmt_name var = match a.o_kind with
    | Flag -> fun n -> strf "$(b,%s)" (esc n)
    | Opt ->
        fun n ->
          if String.length n > 2
          then strf "$(b,%s)=$(i,%s)" (esc n) (esc var)
          else strf "$(b,%s) $(i,%s)" (esc n) (esc var)
    | Opt_vopt _ ->
        fun n ->
          if String.length n > 2
          then strf "$(b,%s)[=$(i,%s)]" (esc n) (esc var)
          else strf "$(b,%s) [$(i,%s)]" (esc n) (esc var)
    in
    let var = if a.docv = "" then "VAL" else a.docv in
    let names = List.sort compare a.o_names in
    let s = String.concat ", " (List.rev_map (fmt_name var) names) in
    s

  let arg_to_man_item ~buf ~subst a =
    let or_env ~value a = match a.env_info with
    | None -> ""
    | Some v ->
        let value = if value then " or" else "absent " in
        strf "%s $(b,%s) env" value (esc v.env_var)
    in
    let absent = match a.absent with
    | Error -> ""
    | Val v ->
        match Lazy.force v with
        | "" -> strf "%s" (or_env ~value:false a)
        | v -> strf "absent=%s%s" v (or_env ~value:true a)
    in
    let optvopt = match a.o_kind with
    | Opt_vopt v -> strf "default=%s" v
    | _ -> ""
    in
    let argvdoc = match optvopt, absent with
    | "", "" -> ""
    | s, "" | "", s -> strf " (%s)" s
    | s, s' -> strf " (%s) (%s)" s s'
    in
    let subst = arg_info_subst ~subst a in
    let doc = Manpage.subst_vars buf ~subst a.doc in
    (a.docs, `I (arg_man_item_label a ^ argvdoc, doc))

  let arg_man_docs ~buf ~subst ei =
    let by_sec_by_arg a a' =
      let c = compare a.docs a'.docs in
      if c <> 0 then c else
      match is_opt a, is_opt a' with
      | true, true -> (* optional by name *)
          let key names =
            let k = String.lowercase (List.hd (List.sort rev_compare names)) in
            if k.[1] = '-' then String.sub k 1 (String.length k - 1) else k
          in
          compare (key a.o_names) (key a'.o_names)
      | false, false -> (* positional by variable *)
          compare (String.lowercase a.docv) (String.lowercase a'.docv)
      | true, false -> -1 (* positional first *)
      | false, true -> 1  (* optional after *)
    in
    let keep_arg a = not (is_pos a && (a.docv = "" || a.doc = "")) in
    let args = List.filter keep_arg (snd ei.term) in
    let args = List.sort by_sec_by_arg args in
    let args = List.rev_map (arg_to_man_item ~buf ~subst) args in
    sorted_items_to_blocks ~boilerplate:None args

  (* Environment doc *)

  let env_boilerplate sec = match sec = Manpage.s_environment with
  | false -> None
  | true -> Some (Manpage.s_environment_intro)

  let env_man_docs ~buf ~subst ~has_senv ei =
    let add_env_man_item ~subst acc e =
      let var = strf "$(b,%s)" @@ esc e.env_var in
      let doc = Manpage.subst_vars buf ~subst e.env_doc in
      (e.env_docs, `I (var, doc)) :: acc
    in
    let add_arg_env acc a = match a.env_info with
    | None -> acc
    | Some e -> add_env_man_item ~subst:(arg_info_subst ~subst a) acc e
    in
    let by_sec_by_rev_name (s0, `I (v0, _)) (s1, `I (v1, _)) =
      let c = compare s0 s1 in
      if c <> 0 then c else compare v1 v0 (* N.B. reverse *)
    in
    let envs = List.fold_left add_arg_env [] (snd ei.term) in
    let envs = List.sort by_sec_by_rev_name envs in
    let envs = (envs :> (string * Manpage.block) list) in
    let boilerplate = if has_senv then None else Some env_boilerplate in
    sorted_items_to_blocks ~boilerplate envs

  (* Man page construction *)

  let ensure_s_name ei sm =
    if Manpage.(smap_has_section sm s_name) then sm else
    let tname = invocation ~sep:'-' ei in
    let tdoc = (fst ei.term).tdoc in
    let tagline = if tdoc = "" then "" else strf " - %s" tdoc in
    let tagline = `P (strf "%s%s" tname tagline) in
    Manpage.(smap_append_block sm ~sec:s_name tagline)

  let ensure_s_synopsis ei sm =
    if Manpage.(smap_has_section sm ~sec:s_synopsis) then sm else
    let synopsis = `P (synopsis ei) in
    Manpage.(smap_append_block sm ~sec:s_synopsis synopsis)

  let insert_term_man_docs ei sm =
    let buf = Buffer.create 200 in
    let subst = term_info_subst ei in
    let ins sm (s, b) = Manpage.smap_append_block sm s b in
    let has_senv = Manpage.(smap_has_section sm s_environment) in
    let sm = List.fold_left ins sm (cmd_man_docs ei) in
    let sm = List.fold_left ins sm (arg_man_docs ~buf ~subst ei) in
    let sm = List.fold_left ins sm (env_man_docs ~buf ~subst ~has_senv ei) in
    sm

  let text ei =
    let sm = Manpage.smap_of_blocks (fst ei.term).man in
    let sm = ensure_s_name ei sm in
    let sm = ensure_s_synopsis ei sm in
    let sm = insert_term_man_docs ei sm in
    Manpage.smap_to_blocks sm

  let title ei =
    let prog = String.capitalize (fst ei.main).name in
    let name = String.uppercase (invocation ~sep:'-' ei) in
    let left_footer = prog ^ match (fst ei.main).version with
    | None -> "" | Some v -> strf " %s" v
    in
    let center_header = strf "%s Manual" prog in
    name, 1, "", left_footer, center_header

  let man ei = title ei, text ei

  let print fmt ppf ei =
    Manpage.print ~subst:(term_info_subst ei) fmt ppf (man ei)

  let pp_synopsis ppf ei =
    let buf = Buffer.create 100 in
    let subst = term_info_subst ei in
    let syn = Manpage.doc_to_plain ~subst buf (synopsis ei) in
    pr ppf "@[%s@]" syn

  let pp_version ppf ei = match (fst ei.main).version with
  | None -> assert false
  | Some v -> pr ppf "@[%a@]@." Manpage.pp_text v
end

(* Errors for the command line user *)

module Err = struct
  let invalid kind s exp = strf "invalid %s %s, %s" kind (quote s) exp
  let invalid_val = invalid "value"
  let no kind s = strf "no %s %s" (quote s) kind
  let not_dir s = strf "%s is not a directory" (quote s)
  let is_dir s = strf "%s is a directory" (quote s)
  let element kind s exp = strf "invalid element in %s (`%s'): %s" kind s exp
  let sep_miss sep s = invalid_val s (strf "missing a `%c' separator" sep)
  let unknown kind ?(hints = []) v =
    let did_you_mean s = strf ", did you mean %s ?" s in
    let hints = match hints with [] -> "." | hs -> did_you_mean (alts_str hs) in
    strf "unknown %s %s%s" kind (quote v) hints

  let ambiguous kind s ambs =
    strf "%s %s ambiguous and could be %s" kind (quote s) (alts_str ambs)

  let pos_excess excess =
    strf "too many arguments, don't know what to do with %s"
      (String.concat ", " (List.map quote excess))

  let flag_value f v =
    strf "option %s is a flag, it cannot take the argument %s"
      (quote f) (quote v)

  let opt_value_missing f = strf "option %s needs an argument" (quote f)
  let opt_parse_value f e = strf "option %s: %s" (quote f) e
  let env_parse_value var e = strf "environment variable %s: %s" (quote var) e
  let opt_repeated f f' =
    if f = f' then strf "option %s cannot be repeated" (quote f) else
    strf "options %s and %s cannot be present at the same time" (quote f)
      (quote f')

  let pos_parse_value a e =
    if a.docv = "" then e else match a.p_kind.pos_len with
    | None -> strf "%s argument: %s" a.docv e
    | Some _ -> strf "%s... arguments: %s" a.docv e

  let arg_missing a =
    if is_opt a then
      let rec long_name = function
      | n :: l -> if (String.length n) > 2 || l = [] then n else long_name l
      | [] -> assert false
      in
      strf "required option %s is missing" (long_name a.o_names)
    else
    if a.docv = "" then strf "a required argument is missing" else
    strf "required argument %s is missing" a.docv

  (* Error printers *)

  let print ppf ei e = pr ppf "%s: @[%a@]@." (fst ei.main).name pp_text e
  let pp_backtrace err ei e bt =
    let bt =
      let len = String.length bt in
      if len > 0 then String.sub bt 0 (len - 1) (* remove final '\n' *) else bt
    in
    pr err
      "%s: @[internal error, uncaught exception:@\n%a@]@."
      (fst ei.main).name pp_lines (strf "%s\n%s" (Printexc.to_string e) bt)

  let pp_try_help ppf ei =
    let exec = Help.invocation ei in
    let main = (fst ei.main).name in
    if exec = main then
      pr ppf "@[<2>Try `%s --help' for more information.@]" exec
    else
    pr ppf "@[<2>Try `%s --help' or `%s --help' for more information.@]"
      exec main

  let pp_usage ppf ei e =
    pr ppf "@[<v>%s: @[%a@]@,@[Usage: @[%a@]@]@,%a@]@."
      (fst ei.main).name pp_text e Help.pp_synopsis ei pp_try_help ei
end

(* Command lines. A command line stores pre-parsed information about
   the command line's arguments in a more structured way. Given the
   [arg_info] values mentioned in a term and Sys.argv (without exec
   name) we parse the command line into a map of [arg_info] values to
   [arg] values. This map is used by the term's closures to retrieve
   and convert command line arguments (see the Arg module). *)

module Cmdline :sig
  exception Error of string
  val choose_term : term_info -> (term_info * 'a) list -> string list ->
    term_info * string list
  val create : ?peek_opts:bool -> arg_info list -> string list -> cmdline
  val opt_arg : cmdline -> arg_info -> (int * string * (string option)) list
  val pos_arg : cmdline -> arg_info -> string list
end = struct
  exception Error of string

  let get_arg cl a = try Amap.find a cl with Not_found -> assert false
  let opt_arg cl a = match get_arg cl a with O l -> l | _ -> assert false
  let pos_arg cl a = match get_arg cl a with P l -> l | _ -> assert false

  let choose_term ti choices = function
  | [] -> ti, []
  | maybe :: args' as args ->
      if String.length maybe > 1 && maybe.[0] = '-' then ti, args else
      let index =
        let add acc (choice, _) = Cmdliner_trie.add acc choice.name choice in
        List.fold_left add Cmdliner_trie.empty choices
      in
      match Cmdliner_trie.find index maybe with
      | `Ok choice -> choice, args'
      | `Not_found ->
        let all = Cmdliner_trie.ambiguities index "" in
        let hints = suggest maybe all in
        raise (Error (Err.unknown "command" ~hints maybe))
      | `Ambiguous ->
          let ambs = Cmdliner_trie.ambiguities index maybe in
          let ambs = List.sort compare ambs in
          raise (Error (Err.ambiguous "command" maybe ambs))

  let arg_info_indexes al =
    (* from [al] returns a trie mapping the names of optional arguments to
       their arg_info, a list with all arg_info for positional arguments and
       a cmdline mapping each arg_info to an empty [arg]. *)
    let rec aux opti posi cl = function
    | a :: l ->
        if is_pos a then aux opti (a :: posi) (Amap.add a (P []) cl) l else
        let add t name = Cmdliner_trie.add t name a in
        aux (List.fold_left add opti a.o_names) posi (Amap.add a (O []) cl) l
    | [] -> opti, posi, cl
    in
    aux Cmdliner_trie.empty [] Amap.empty al

  let parse_opt_arg s =          (* (name,value) of opt arg, assert len > 1. *)
    let l = String.length s in
    if s.[1] <> '-' then
      if l = 2 then s, None else
      String.sub s 0 2, Some (String.sub s 2 (l - 2))
    else try
      let i = String.index s '=' in
      String.sub s 0 i, Some (String.sub s (i + 1) (l - i - 1))
    with Not_found -> s, None

  let parse_args ~peek_opts opti cl args =
    (* returns an updated [cl] cmdline according to the options found in [args]
       with the trie index [opti]. Positional arguments are returned in order
       in a list. *)
    let rec aux k opti cl pargs = function
    | [] -> cl, (List.rev pargs)
    | "--" :: args -> cl, (List.rev_append pargs args)
    | s :: args ->
        let is_opt s = String.length s > 1 && s.[0] = '-' in
        let is_short_opt s = String.length s = 2 && s.[0] = '-' in
        if not (is_opt s) then aux (k+1) opti cl (s :: pargs) args else
        let name, value = parse_opt_arg s in
        match Cmdliner_trie.find opti name with
        | `Ok a ->
            let value, args = match value, a.o_kind with
            | Some v, Flag when is_short_opt name -> None, ("-" ^ v) :: args
            | Some v, _ -> value, args
            | None, Flag -> value, args
            | None, _ ->
                match args with
                | v :: rest -> if is_opt v then None, args else Some v, rest
                | [] -> None, args
            in
            let arg = O ((k, name, value) :: opt_arg cl a) in
            aux (k+1) opti (Amap.add a arg cl) pargs args
        | `Not_found when peek_opts -> aux (k+1) opti cl pargs args (* skip *)
        | `Not_found ->
            let hints =
              if String.length s <= 2 then [] else
              let short_opt, long_opt =
                if s.[1] <> '-'
                then s, Printf.sprintf "-%s" s
                else String.sub s 1 (String.length s - 1), s
              in
              let short_opt, _ = parse_opt_arg short_opt in
              let long_opt, _ = parse_opt_arg long_opt in
              let all = Cmdliner_trie.ambiguities opti "-" in
              match List.mem short_opt all, suggest long_opt all with
              | false, [] -> []
              | false, l -> l
              | true, [] -> [short_opt]
              | true, l -> if List.mem short_opt l then l else short_opt :: l
            in
            raise (Error (Err.unknown "option" ~hints name))
        | `Ambiguous ->
            let ambs = Cmdliner_trie.ambiguities opti name in
            let ambs = List.sort compare ambs in
            raise (Error (Err.ambiguous "option" name ambs))
    in
    aux 0 opti cl [] args

  let take_range start stop l =
    let rec loop i acc = function
    | [] -> List.rev acc
    | v :: vs ->
        if i < start then loop (i + 1) acc vs else
        if i <= stop then loop (i + 1) (v :: acc) vs else
        List.rev acc
    in
    loop 0 [] l

  let process_pos_args posi cl pargs =
    (* returns an updated [cl] cmdline in which each positional arg mentioned
       in the list index posi, is given a value according the list
       of positional arguments values [pargs]. *)
    if pargs = [] then cl else
    let last = List.length pargs - 1 in
    let pos rev k = if rev then last - k else k in
    let rec loop cl max_spec = function
    | [] -> cl, max_spec
    | a :: al ->
        let rev = a.p_kind.pos_rev in
        let start = pos rev a.p_kind.pos_start in
        let stop = match a.p_kind.pos_len with
        | None -> pos rev last
        | Some n -> pos rev (a.p_kind.pos_start + n - 1)
        in
        let start, stop = if rev then stop, start else start, stop in
        let arg = P (take_range start stop pargs) in
        let max_spec = max stop max_spec in
        let cl = Amap.add a arg cl in
        loop cl max_spec al
    in
    let cl, max_spec = loop cl (-1) posi in
    if last <= max_spec then cl else
    let excess = take_range (max_spec + 1) last pargs in
    raise (Error (Err.pos_excess excess))

  let create ?(peek_opts = false) al args =
    let opti, posi, cl = arg_info_indexes al in
    let cl, pargs = parse_args ~peek_opts opti cl args in
    if peek_opts then cl (* skip positional arguments *) else
    process_pos_args posi cl pargs
end

module Arg = struct
  type 'a parser = string -> [ `Ok of 'a | `Error of string ]
  type 'a printer = Format.formatter -> 'a -> unit
  type 'a converter = 'a parser * 'a printer
  type env = env_info
  type 'a arg_converter = (eval_info -> cmdline -> 'a)
  type 'a t = arg_info list * 'a arg_converter
  type info = arg_info

  let env_var
      ?(docs = Manpage.s_environment) ?(doc = "See option $(opt).") env_var =
    { env_var = env_var; env_doc = doc; env_docs = docs }

  let ( & ) f x = f x
  let parse_error e = raise (Cmdline.Error e)
  let some ?(none = "") (parse, print) =
    (fun s -> match parse s with `Ok v -> `Ok (Some v) | `Error _ as e -> e),
    (fun ppf v -> match v with None -> pp_str ppf none| Some v -> print ppf v)


  let dumb_p_kind = { pos_rev = false; pos_start = -1; pos_len = None }

  let info ?docs ?(docv = "") ?(doc = "") ?env names =
    let dash n = if String.length n = 1 then "-" ^ n else "--" ^ n in
    let docs = match docs with
    | None -> if names = [] then Manpage.s_arguments else Manpage.s_options
    | Some s -> s
    in
    { id = arg_id (); absent = Val (lazy "");
      env_info = env;
      doc = doc; docv = docv; docs = docs;
      p_kind = dumb_p_kind; o_kind = Flag; o_names = List.rev_map dash names;
      o_all = false; }

  let env_bool_parse s = match String.lowercase s with
  | "" | "false" | "no" | "n" | "0" -> `Ok false
  | "true" | "yes" | "y" | "1" -> `Ok true
  | s -> `Error (Err.invalid_val s (alts_str ["true"; "yes"; "false"; "no" ]))

  let parse_to_list parser s = match parser s with
  | `Ok v -> `Ok [v]
  | `Error _ as e -> e

  let try_env ei a parse ~absent = match a.env_info with
  | None -> absent
  | Some env ->
      match ei.env env.env_var with
      | None -> absent
      | Some v ->
          match parse v with
          | `Ok v -> v
          | `Error e ->
              parse_error (Err.env_parse_value env.env_var e)

  let flag a =
    if is_pos a then invalid_arg err_not_opt else
    let convert ei cl = match Cmdline.opt_arg cl a with
    | [] -> try_env ei a env_bool_parse ~absent:false
    | [_, _, None] -> true
    | [_, f, Some v] -> parse_error (Err.flag_value f v)
    | (_, f, _) :: (_ ,g, _) :: _  -> parse_error (Err.opt_repeated f g)
    in
    [a], convert

  let flag_all a =
    if is_pos a then invalid_arg err_not_opt else
    let a = { a with o_all = true } in
    let convert ei cl = match Cmdline.opt_arg cl a with
    | [] -> try_env ei a (parse_to_list env_bool_parse) ~absent:[]
    | l ->
        let truth (_, f, v) = match v with
        | None -> true | Some v -> parse_error (Err.flag_value f v)
        in
        List.rev_map truth l
    in
    [a], convert

  let vflag v l =
    let convert _ cl =
      let rec aux fv = function
      | (v, a) :: rest ->
          begin match Cmdline.opt_arg cl a with
          | [] -> aux fv rest
          | [_, f, None] ->
              begin match fv with
              | None -> aux (Some (f, v)) rest
              | Some (g, _) -> parse_error (Err.opt_repeated g f)
              end
          | [_, f, Some v] -> parse_error (Err.flag_value f v)
          | (_, f, _) :: (_, g, _) :: _ -> parse_error (Err.opt_repeated g f)
          end
      | [] -> match fv with None -> v | Some (_, v) -> v
      in
      aux None l
    in
    let flag (_, a) = if is_pos a then invalid_arg err_not_opt else a in
    List.rev_map flag l, convert

  let vflag_all v l =
    let convert _ cl =
      let rec aux acc = function
      | (fv, a) :: rest ->
          begin match Cmdline.opt_arg cl a with
          | [] -> aux acc rest
          | l ->
              let fval (k, f, v) = match v with
              | None -> (k, fv) | Some v -> parse_error (Err.flag_value f v)
              in
              aux (List.rev_append (List.rev_map fval l) acc) rest
          end
      | [] ->
          if acc = [] then v else List.rev_map snd (List.sort rev_compare acc)
      in
      aux [] l
    in
    let flag (_, a) =
      if is_pos a then invalid_arg err_not_opt else { a with o_all = true }
    in
    List.rev_map flag l, convert

  let parse_opt_value parse f v = match parse v with
  | `Ok v -> v | `Error e -> parse_error (Err.opt_parse_value f e)

  let opt ?vopt (parse, print) v a =
    if is_pos a then invalid_arg err_not_opt else
    let a = { a with absent = Val (lazy (str_of_pp print v));
                     o_kind = match vopt with
                     | None -> Opt | Some dv -> Opt_vopt (str_of_pp print dv) }
    in
    let convert ei cl = match Cmdline.opt_arg cl a with
    | [] -> try_env ei a parse ~absent:v
    | [_, f, Some v] -> parse_opt_value parse f v
    | [_, f, None] ->
        begin match vopt with
        | None -> parse_error (Err.opt_value_missing f)
        | Some optv -> optv
        end
    | (_, f, _) :: (_, g, _) :: _ -> parse_error (Err.opt_repeated g f)
    in
    [a], convert

  let opt_all ?vopt (parse, print) v a =
    if is_pos a then invalid_arg err_not_opt else
    let a = { a with absent = Val (lazy ""); o_all = true;
                     o_kind = match vopt with
                     | None -> Opt | Some dv -> Opt_vopt (str_of_pp print dv) }
    in
    let convert ei cl = match Cmdline.opt_arg cl a with
    | [] -> try_env ei a (parse_to_list parse) ~absent:v
    | l ->
        let parse (k, f, v) = match v with
        | Some v -> (k, parse_opt_value parse f v)
        | None -> match vopt with
        | None -> parse_error (Err.opt_value_missing f)
        | Some dv -> (k, dv)
        in
        List.rev_map snd (List.sort rev_compare (List.rev_map parse l))
    in
    [a], convert

  (* Positional arguments *)

  let parse_pos_value parse a v = match parse v with
  | `Ok v -> v | `Error e -> parse_error (Err.pos_parse_value a e)

  let pos ?(rev = false) k (parse, print) v a =
    if is_opt a then invalid_arg err_not_pos else
    let p_kind = { pos_rev = rev; pos_start = k; pos_len = Some 1 } in
    let absent = Val (lazy (str_of_pp print v)) in
    let a = { a with p_kind; absent } in
    let convert ei cl = match Cmdline.pos_arg cl a with
    | [] -> try_env ei a parse ~absent:v
    | [v] -> parse_pos_value parse a v
    | _ -> assert false
    in
    [a], convert

  let pos_list kind (parse, _) v a =
    if is_opt a then invalid_arg err_not_pos else
    let a = { a with p_kind = kind } in
    let convert ei cl = match Cmdline.pos_arg cl a with
    | [] -> try_env ei a (parse_to_list parse) ~absent:v
    | l -> List.rev (List.rev_map (parse_pos_value parse a) l)
    in
    [a], convert

  let pos_all =
    let all = { pos_rev = false; pos_start = 0; pos_len = None } in
    fun c v a -> pos_list all c v a

  let pos_left ?(rev = false) k =
    let pos_start = if rev then k + 1 else 0 in
    let pos_len = if rev then None else Some k in
    pos_list { pos_rev = rev; pos_start; pos_len }

  let pos_right ?(rev = false) k =
    let pos_start = if rev then 0 else k + 1 in
    let pos_len = if rev then Some k else None in
    pos_list { pos_rev = rev; pos_start; pos_len }

  (* Arguments as terms *)

  let absent_error al = List.rev_map (fun a -> { a with absent = Error }) al
  let value a = a
  let required (al, convert) =
    let al = absent_error al in
    let convert ei cl = match convert ei cl with
    | Some v -> v
    | None -> parse_error (Err.arg_missing (List.hd al))
    in
    al, convert

  let non_empty (al, convert) =
    let al = absent_error al in
    let convert ei cl = match convert ei cl with
    | [] -> parse_error (Err.arg_missing (List.hd al))
    | l -> l
    in
    al, convert

  let last (al, convert) =
    let convert ei cl = match convert ei cl with
    | [] -> parse_error (Err.arg_missing (List.hd al))
    | l -> List.hd (List.rev l)
    in
    al, convert

  (* Predefined converters. *)

  let bool =
    (fun s -> try `Ok (bool_of_string s) with Invalid_argument _ ->
        `Error (Err.invalid_val s (alts_str ["true"; "false"]))),
    Format.pp_print_bool

  let char =
    (fun s -> if String.length s = 1 then `Ok s.[0] else
      `Error (Err.invalid_val s "expected a character")),
    pp_char

  let parse_with t_of_str exp s =
    try `Ok (t_of_str s) with Failure _ -> `Error (Err.invalid_val s exp)

  let int =
    parse_with int_of_string "expected an integer", Format.pp_print_int

  let int32 =
    parse_with Int32.of_string "expected a 32-bit integer",
    (fun ppf -> pr ppf "%ld")

  let int64 =
    parse_with Int64.of_string "expected a 64-bit integer",
    (fun ppf -> pr ppf "%Ld")

  let nativeint =
    parse_with Nativeint.of_string "expected a processor-native integer",
    (fun ppf -> pr ppf "%nd")

  let float =
    parse_with float_of_string "expected a floating point number",
    Format.pp_print_float

  let string = (fun s -> `Ok s), pp_str
  let enum sl =
    if sl = [] then invalid_arg err_empty_list else
    let t = Cmdliner_trie.of_list sl in
    let parse s = match Cmdliner_trie.find t s with
    | `Ok _ as r -> r
    | `Ambiguous ->
        let ambs = List.sort compare (Cmdliner_trie.ambiguities t s) in
        `Error (Err.ambiguous "enum value" s ambs)
    | `Not_found ->
        let alts = List.rev (List.rev_map (fun (s, _) -> s) sl) in
        `Error (Err.invalid_val s ("expected " ^ (alts_str alts)))
    in
    let print ppf v =
      let sl_inv = List.rev_map (fun (s,v) -> (v,s)) sl in
      try pp_str ppf (List.assoc v sl_inv)
      with Not_found -> invalid_arg err_incomplete_enum
    in
    parse, print

  let file =
    (fun s -> if Sys.file_exists s then `Ok s else
      `Error (Err.no "file or directory" s)),
    pp_str

  let dir =
    (fun s ->
       if Sys.file_exists s then
         if Sys.is_directory s then `Ok s else `Error (Err.not_dir s)
       else
       `Error (Err.no "directory" s)),
    pp_str

  let non_dir_file =
    (fun s ->
       if Sys.file_exists s then
         if not (Sys.is_directory s) then `Ok s else `Error (Err.is_dir s)
       else
       `Error (Err.no "file" s)),
    pp_str

  let split_and_parse sep parse s =
    let parse sub = match parse sub with
    | `Error e -> failwith e | `Ok v -> v in
    let rec split accum j =
      let i = try String.rindex_from s j sep with Not_found -> -1 in
      if (i = -1) then
        let p = String.sub s 0 (j + 1) in
        if p <> "" then parse p :: accum else accum
      else
      let p = String.sub s (i + 1) (j - i) in
      let accum' = if p <> "" then parse p :: accum else accum in
      split accum' (i - 1)
    in
    split [] (String.length s - 1)

  let list ?(sep = ',') (parse, pp_e) =
    let parse s = try `Ok (split_and_parse sep parse s) with
    | Failure e -> `Error (Err.element "list" s e)
    in
    let rec print ppf = function
    | v :: l -> pp_e ppf v; if (l <> []) then (pp_char ppf sep; print ppf l)
    | [] -> ()
    in
    parse, print

  let array ?(sep = ',') (parse, pp_e) =
    let parse s = try `Ok (Array.of_list (split_and_parse sep parse s)) with
    | Failure e -> `Error (Err.element "array" s e)
    in
    let print ppf v =
      let max = Array.length v - 1 in
      for i = 0 to max do pp_e ppf v.(i); if i <> max then pp_char ppf sep done
    in
    parse, print

  let split_left sep s =
    try
      let i = String.index s sep in
      let len = String.length s in
      Some ((String.sub s 0 i), (String.sub s (i + 1) (len - i - 1)))
    with Not_found -> None

  let pair ?(sep = ',') (pa0, pr0) (pa1, pr1) =
    let parser s = match split_left sep s with
    | None -> `Error (Err.sep_miss sep s)
    | Some (v0, v1) ->
        match pa0 v0, pa1 v1 with
        | `Ok v0, `Ok v1 -> `Ok (v0, v1)
        | `Error e, _ | _, `Error e -> `Error (Err.element "pair" s e)
    in
    let printer ppf (v0, v1) = pr ppf "%a%c%a" pr0 v0 sep pr1 v1 in
    parser, printer

  let t2 = pair
  let t3 ?(sep = ',') (pa0, pr0) (pa1, pr1) (pa2, pr2) =
    let parse s = match split_left sep s with
    | None -> `Error (Err.sep_miss sep s)
    | Some (v0, s) ->
        match split_left sep s with
        | None -> `Error (Err.sep_miss sep s)
        | Some (v1, v2) ->
            match pa0 v0, pa1 v1, pa2 v2 with
            | `Ok v0, `Ok v1, `Ok v2 -> `Ok (v0, v1, v2)
            | `Error e, _, _ | _, `Error e, _ | _, _, `Error e ->
                `Error (Err.element "triple" s e)
    in
    let print ppf (v0, v1, v2) =
      pr ppf "%a%c%a%c%a" pr0 v0 sep pr1 v1 sep pr2 v2
    in
    parse, print

  let t4 ?(sep = ',') (pa0, pr0) (pa1, pr1) (pa2, pr2) (pa3, pr3) =
    let parse s = match split_left sep s with
    | None -> `Error (Err.sep_miss sep s)
    | Some(v0, s) ->
        match split_left sep s with
        | None -> `Error (Err.sep_miss sep s)
        | Some (v1, s) ->
            match split_left sep s with
            | None -> `Error (Err.sep_miss sep s)
            | Some (v2, v3) ->
                match pa0 v0, pa1 v1, pa2 v2, pa3 v3 with
                | `Ok v1, `Ok v2, `Ok v3, `Ok v4 -> `Ok (v1, v2, v3, v4)
                | `Error e, _, _, _ | _, `Error e, _, _ | _, _, `Error e, _
                | _, _, _, `Error e -> `Error (Err.element "quadruple" s e)
    in
    let print ppf (v0, v1, v2, v3) =
      pr ppf "%a%c%a%c%a%c%a" pr0 v0 sep pr1 v1 sep pr2 v2 sep pr3 v3
    in
    parse, print

  (* Documentation formatting helpers *)

  let doc_quote = quote
  let doc_alts = alts_str
  let doc_alts_enum ?quoted enum = alts_str ?quoted (List.map fst enum)
end

module Term = struct
  type info = term_info
  type +'a t = arg_info list * (eval_info -> cmdline -> 'a)
  type 'a result = [
    | `Ok of 'a | `Error of [`Parse | `Term | `Exn ] | `Version | `Help ]

  exception Term of
      [ `Help of Manpage.format * string option
      | `Error of bool * string ]

  let info
      ?(sdocs = Manpage.s_options) ?(man = []) ?(docs = "COMMANDS") ?(doc = "")
      ?version name =
    { name = name; version = version; tdoc = doc; tdocs = docs; sdocs = sdocs;
      man = man }

  let name ti = ti.name
  let const v = [], (fun _ _ -> v)
  let pure (* deprecated *) = const
  let app (al, f) (al', v) =
    List.rev_append al al',
    fun ei cl -> (f ei cl) (v ei cl)

  let ( $ ) = app

  type 'a ret =
    [ `Help of Manpage.format * string option
    | `Error of (bool * string)
    | `Ok of 'a ]

  let ret (al, v) =
    al, fun ei cl -> match v ei cl with
    | `Ok v -> v
    | `Error (u,e) -> raise (Term (`Error (u,e)))
    | `Help h -> raise (Term (`Help h))

  let main_name = [], (fun ei _ -> (fst ei.main).name)
  let choice_names =
    [], fun ei _ -> List.rev_map (fun e -> (fst e).name) ei.choices

  let man_fmts =
    ["auto", `Auto; "pager", `Pager; "groff", `Groff; "plain", `Plain]

  let man_fmts_enum = Arg.enum man_fmts
  let man_fmts_alts = Arg.doc_alts_enum man_fmts
  let man_fmts_doc kind =
    strf "Show %s in format $(docv). The value $(docv) must be %s. With `auto',
          the format is `pager` or `plain' whenever the $(b,TERM) env var is
          `dumb' or undefined."
      kind man_fmts_alts

  let man_format =
    let doc = man_fmts_doc "output" in
    let docv = "FMT" in
    Arg.(value & opt man_fmts_enum `Pager & info ["man-format"] ~docv ~doc)

  (* Evaluation *)

  let remove_exec argv =
    try List.tl (Array.to_list argv) with Failure _ -> invalid_arg err_argv

  let add_std_opts ei =
    let docs = (fst ei.term).sdocs in
    let args, v_lookup =
      if (fst ei.main).version = None then [], None else
      let (a, lookup) =
        Arg.flag (Arg.info ["version"] ~docs ~doc:"Show version information.")
      in
      a, Some lookup
    in
    let args, h_lookup =
      let (a, lookup) =
        let doc = man_fmts_doc "this help" in
        let a = Arg.info ["help"] ~docv:"FMT" ~docs ~doc in
        Arg.opt ~vopt:(Some `Auto) (Arg.some man_fmts_enum) None a
      in
      List.rev_append a args, lookup
    in
    h_lookup, v_lookup,
    { ei with term = (fst ei.term), List.rev_append args (snd ei.term) }

  let eval_term help err ei f args =
    let help_arg, vers_arg, ei = add_std_opts ei in
    try
      let cl = Cmdline.create (snd ei.term) args in
      match help_arg ei cl, vers_arg with
      | Some fmt, _ -> Help.print fmt help ei; `Help
      | None, Some v_arg when v_arg ei cl -> Help.pp_version help ei; `Version
      | _ -> `Ok (f ei cl)
    with
    | Cmdline.Error e -> Err.pp_usage err ei e; `Error `Parse
    | Term (`Error (usage, e)) ->
        if usage then Err.pp_usage err ei e else Err.print err ei e;
        `Error `Term
    | Term (`Help (fmt, cmd)) ->
        let ei = match cmd with
        | Some cmd ->
            let cmd =
              try List.find (fun (i, _) -> i.name = cmd) ei.choices
              with Not_found -> invalid_arg (err_help cmd)
            in
            {ei with term = cmd }
        | None -> { ei with term = ei.main }
        in
        let _, _, ei = add_std_opts ei in
        Help.print fmt help ei; `Help

  let env_default v = try Some (Sys.getenv v) with Not_found -> None

  let eval ?(help = Format.std_formatter) ?(err = Format.err_formatter)
      ?(catch = true) ?(env = env_default) ?(argv = Sys.argv) ((al, f), ti) =
    let term = ti, al in
    let ei = { term = term; main = term; choices = []; env = env } in
    try eval_term help err ei f (remove_exec argv) with
    | e when catch ->
        Err.pp_backtrace err ei e (Printexc.get_backtrace ()); `Error `Exn

  let eval_choice ?(help = Format.std_formatter) ?(err = Format.err_formatter)
      ?(catch = true) ?(env = env_default) ?(argv = Sys.argv)
      (((al, f) as t), ti) choices =
    let ei_choices = List.rev_map (fun ((al, _), ti) -> ti, al) choices in
    let main = (ti, al) in
    let ei = { term = main; main = main; choices = ei_choices; env = env } in
    try
      let chosen, args = Cmdline.choose_term ti ei_choices (remove_exec argv) in
      let find_chosen (_, ti) = ti = chosen in
      let (al, f), _ = List.find find_chosen ((t, ti) :: choices) in
      let ei = { ei with term = (chosen, al) } in
      eval_term help err ei f args
    with
    | Cmdline.Error e ->                    (* may be raised by choose_term. *)
        Err.pp_usage err ei e; `Error `Parse
    | e when catch ->
        Err.pp_backtrace err ei e (Printexc.get_backtrace ()); `Error `Exn

  let eval_peek_opts ?(version_opt = false) ?(env = env_default)
      ?(argv = Sys.argv) (al, f) =
    let args = remove_exec argv in
    let version = if version_opt then Some "dummy" else None in
    let term = info ?version "dummy", al in
    let ei = { term = term; main = term; choices = []; env = env } in
    let help_arg, vers_arg, ei = add_std_opts ei in
    try
      let cl = Cmdline.create ~peek_opts:true (snd ei.term) args in
      match help_arg ei cl, vers_arg with
      | Some fmt, _ ->
          (try (Some (f ei cl), `Help) with e -> None, `Help)
      | None, Some v_arg when v_arg ei cl ->
          (try (Some (f ei cl), `Version) with e -> None, `Version)
      | _ ->
          let v = f ei cl in
          Some v, `Ok v
    with
    | Cmdline.Error _ -> None, (`Error `Parse)
    | Term _ -> None, (`Error `Term)
    | e -> None, (`Error `Exn)
end

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
