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

type pos_kind =                            (* kinds of positional arguments. *)
  | All                                         (* all positional arguments. *)
  | Nth of bool * int                                  (* specific position. *)
  | Left of bool * int                (* all args on the left of a position. *)
  | Right of bool * int              (* all args on the right of a position. *)

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

  let esc = Cmdliner_manpage.markup_text_escape

  let ei_subst ei = function
  | "tname" -> Some (strf "$(b,%s)" @@ esc (fst ei.term).name)
  | "mname" -> Some (strf "$(b,%s)" @@ esc (fst ei.main).name)
  | _ -> None

  let invocation ?(sep = ' ') ei = match eval_kind ei with
  | `Simple | `M_main -> (fst ei.main).name
  | `M_choice -> strf "%s%c%s" (fst ei.main).name sep (fst ei.term).name

  let title ei =
    let prog = String.capitalize (fst ei.main).name in
    let name = String.uppercase (invocation ~sep:'-' ei) in
    let left_footer = prog ^ match (fst ei.main).version with
      | None -> "" | Some v -> strf " %s" v
    in
    let center_header = strf "%s Manual" prog in
    name, 1, "", left_footer, center_header

  let name_section ei =
    let tdoc d = if d = "" then "" else (strf " - %s" d) in
    [`S Manpage.s_name;
     `P (strf "%s%s" (invocation ~sep:'-' ei) (tdoc (fst ei.term).tdoc)); ]

  let synopsis ei = match eval_kind ei with
  | `M_main -> strf "$(b,%s) $(i,COMMAND) ..." @@ esc (invocation ei)
  | `Simple | `M_choice ->
      let rev_cmp (p, _) (p', _) = match p', p with        (* best effort. *)
      | p, All -> -1 | All, p -> 1
      | Left _, Right _ -> -1 | Right _, Left _ -> 1
      | Left (false, k), Nth (false, k')
      | Nth (false, k), Nth (false, k')
      | Nth (false, k), Right (false, k') -> if k <= k' then -1 else 1
      | Nth (false, k), Left (false, k')
      | Right (false, k), Nth (false, k') -> if k >= k' then 1 else -1
      | Left (true, k), Nth (true, k')
      | Nth (true, k), Nth (true, k')
      | Nth (true, k), Right (true, k') -> if k >= k' then -1 else 1
      | Nth (true, k), Left (true, k')
      | Right (true, k), Nth (true, k') -> if k <= k' then 1 else -1
      | p, p' -> compare p p'
      in
      let rec format_pos acc = function
      | a :: al ->
          if is_opt a then format_pos acc al else
          let v =
            if a.docv = "" then "$(i,ARG)" else strf "$(i,%s)" (esc a.docv)
          in
          let v = if a.absent = Error then strf "%s" v else strf "[%s]" v in
          let v = v ^ match a.p_kind with Nth _ -> "" | _ -> "..." in
          format_pos ((a.p_kind, v) :: acc) al
      | [] -> acc
      in
      let args = List.sort rev_cmp (format_pos [] (snd ei.term)) in
      let args = String.concat " " (List.rev_map snd args) in
      strf "$(b,%s) [$(i,OPTION)]... %s" (esc @@ invocation ei) args

  let get_synopsis_section ei =
    let rec split_section syn = function
    | (`S _ :: _ | []) as man -> List.rev syn, man
    |  b :: bs -> split_section (b :: syn) bs
    in
    match (fst ei.term).man with
    | `S syn as s :: rest when syn = Manpage.s_synopsis -> (* user-defined *)
        split_section [s] rest
    | man -> (* automatic *)
        [ `S Manpage.s_synopsis; `P (synopsis ei); ], man

  let make_arg_label a =
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

  let arg_info_substs ~subst ~buf a doc =
    let subst = function
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
    in
    Manpage.subst_vars buf ~subst doc

  let or_env ~value a = match a.env_info with
  | None -> ""
  | Some v ->
      let value = if value then " or" else "absent " in
      strf "%s $(b,%s) env" value (esc v.env_var)

  let make_arg_items ei =
    let subst = ei_subst ei in
    let buf = Buffer.create 200 in
    let cmp a a' =
      let c = compare a.docs a'.docs in
      if c <> 0 then c else
      match is_opt a, is_opt a' with
      | true, true ->
          let key names =
            let k = String.lowercase (List.hd (List.sort rev_compare names)) in
            if k.[1] = '-' then String.sub k 1 (String.length k - 1) else k
          in
          compare (key a.o_names) (key a'.o_names)
      | false, false ->
          compare (String.lowercase a.docv) (String.lowercase a'.docv)
      | true, false -> -1
      | false, true -> 1
    in
    let format a =
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
      (a.docs, `I (make_arg_label a ^ argvdoc,
                   (arg_info_substs ~subst ~buf a a.doc)))
    in
    let is_arg_item a = not (is_pos a && (a.docv = "" || a.doc = "")) in
    let l = List.sort cmp (List.filter is_arg_item (snd ei.term)) in
    List.rev_map format l

  let make_env_items_rev ei =
    let subst = ei_subst ei in
    let buf = Buffer.create 200 in
    let cmp a a' =
      let e' = match a'.env_info with None -> assert false | Some a' -> a' in
      let e = match a.env_info with None -> assert false | Some a -> a in
      let c = compare e.env_docs e'.env_docs in
      if c <> 0 then c else
      compare e.env_var e'.env_var
    in
    let format a =
      let e = match a.env_info with None -> assert false | Some a -> a in
      (e.env_docs,
       `I (strf "$(b,%s)" e.env_var, arg_info_substs ~subst ~buf a e.env_doc))
    in
    let is_env_item a = a.env_info <> None in
    let l = List.sort cmp (List.filter is_env_item (snd ei.term)) in
    List.rev_map format l

  let make_cmd_items ei = match eval_kind ei with
  | `Simple | `M_choice -> []
  | `M_main ->
      let add_cmd acc (ti, _) =
        (ti.tdocs, `I ((strf "$(b,%s)" @@ esc ti.name), ti.tdoc)) :: acc
      in
      List.sort rev_compare (List.fold_left add_cmd [] ei.choices)

  let text ei =                  (* man that code is particulary unreadable. *)
    let rec merge_items acc to_insert mark il = function
    | `S s as sec :: ts ->
        let acc = List.rev_append to_insert acc in
        let acc = if mark then sec :: `Orphan_mark :: acc else sec :: acc in
        let to_insert, il = List.partition (fun (n, _) -> n = s) il in
        let to_insert = List.rev_map (fun (_, i) -> i) to_insert in
        let to_insert = (to_insert :> [ `Orphan_mark | Manpage.block] list) in
        merge_items acc to_insert (s = Manpage.s_description) il ts
    | t :: ts ->
        let t = (t :> [`Orphan_mark | Manpage.block]) in
        merge_items (t :: acc) to_insert mark il ts
    | [] ->
        let acc = List.rev_append to_insert acc in
        (if mark then `Orphan_mark :: acc else acc), il
    in
    let rec merge_orphans acc orphans = function
    | `Orphan_mark :: ts ->
        let rec merge acc s = function
        | [] -> (`S s) :: acc
        | (s', i) :: ss ->
            let i = (i :> Manpage.block) in
            if s = s' then merge (i :: acc) s ss else
            merge (i :: (`S s) :: acc) s' ss
        in
        let acc = match orphans with
        | [] -> acc | (s, _) :: _ -> merge acc s orphans
        in
        merge_orphans acc [] ts
    | (#Manpage.block as e) :: ts -> merge_orphans (e :: acc) orphans ts
    | [] -> acc
    in
    let cmds = make_cmd_items ei in
    let args = make_arg_items ei in
    let envs_rev = make_env_items_rev ei in
    let items_rev = List.rev_append cmds (List.rev_append args envs_rev) in
    let cmp (s, _) (s', _) = match s, s with
    | "ENVIRONMENT VARIABLES", _ -> 1  (* Put env vars at the end. *)
    | s, "ENVIRONMENT VARIABLES" -> -1
    | s, s' -> compare s s' (* other predefined sec. names order correctly *)
    in
    let items = List.rev (List.stable_sort cmp items_rev) in
    let synopsis, man = get_synopsis_section ei in
    let rev_text, orphans = merge_items [`Orphan_mark] [] false items man in
    synopsis @ merge_orphans [] orphans rev_text

  let man ei = title ei, (name_section ei) @ (text ei)

  let print fmt ppf ei = Manpage.print ~subst:(ei_subst ei) fmt ppf (man ei)
  let pp_synopsis ppf ei =
    let buf = Buffer.create 100 in
    let syn = Manpage.doc_to_plain ~subst:(ei_subst ei) buf (synopsis ei) in
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
    if a.docv = "" then e else match a.p_kind with
    | Nth _ -> strf "%s argument: %s" a.docv e
    | _ -> strf "%s... arguments: %s" a.docv e

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

  let opt_arg cl a = match try Amap.find a cl with Not_found -> assert false
  with O l -> l | _ -> assert false

  let pos_arg cl a = match try Amap.find a cl with Not_found -> assert false
  with P l -> l | _ -> assert false

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

  let process_pos_args posi cl pargs =
    (* returns an updated [cl] cmdline in which each positional arg mentioned
       in the list index posi, is given a value according the list
       of positional arguments values [pargs]. *)
    if pargs = [] then cl else
    let rec take n acc l =
      if n = 0 then List.rev acc else
      take (n - 1) (List.hd l :: acc) (List.tl l)
    in
    let rec aux pargs last cl max_spec = function
    | a :: al ->
        let arg, max_spec = match a.p_kind with
        | All -> P pargs, last
        | Nth (rev, k) ->
            let k = if rev then last - k else k in
            let max_spec = max k max_spec in
            if k < 0 || k > last then P [], max_spec else
            P ([List.nth pargs k]), max_spec
        | Left (rev, k) ->
            let k = if rev then last - k else k in
            let max_spec = max k max_spec in
            if k <= 0 || k > last then P [], max_spec else
            P (take k [] pargs), max_spec
        | Right (rev, k) ->
            let k = if rev then last - k else k in
            if k < 0 || k >= last then P [], last else
            P (List.rev (take (last - k) [] (List.rev pargs))), last
        in
        aux pargs last (Amap.add a arg cl) max_spec al
    | [] -> cl, max_spec
    in
    let last = List.length pargs - 1 in
    let cl, max_spec = aux pargs last cl (-1) posi in
    if last <= max_spec then cl else
    let excess = List.rev (take (last - max_spec) [] (List.rev pargs)) in
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

  let info ?docs ?(docv = "") ?(doc = "") ?env names =
    let dash n = if String.length n = 1 then "-" ^ n else "--" ^ n in
    let docs = match docs with
    | None -> if names = [] then Manpage.s_arguments else Manpage.s_options
    | Some s -> s
    in
    { id = arg_id (); absent = Val (lazy "");
      env_info = env;
      doc = doc; docv = docv; docs = docs;
      p_kind = All; o_kind = Flag; o_names = List.rev_map dash names;
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
    let a = { a with p_kind = Nth (rev, k);
                     absent = Val (lazy (str_of_pp print v)) }
    in
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

  let pos_all c v a = pos_list All c v a
  let pos_left ?(rev = false) k = pos_list (Left (rev, k))
  let pos_right ?(rev = false) k = pos_list (Right (rev, k))

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
