(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Manpages *)

module Manpage = Cmdliner_manpage

(* Command lines. A command line stores pre-parsed information about
   the command line's arguments in a more structured way. Given the
   [arg_info] values mentioned in a term and Sys.argv (without exec
   name) we parse the command line into a map of [arg_info] values to
   [arg] values. This map is used by the term's closures to retrieve
   and convert command line arguments (see the Arg module). *)

module Cmdline :sig
  type t
  val create :
    ?peek_opts:bool -> Cmdliner_info.arg list -> string list ->
    (t, string * t) result

  val opt_arg : t -> Cmdliner_info.arg -> (int * string * (string option)) list
  val pos_arg : t -> Cmdliner_info.arg -> string list
end = struct

  module Arg_info = struct
    type t = Cmdliner_info.arg
    let compare a0 a1 =
      compare (Cmdliner_info.arg_id a0) (Cmdliner_info.arg_id a1)
  end
  module Amap = Map.Make (Arg_info)

  type arg =      (* unconverted argument data as found on the command line. *)
  | O of (int * string * (string option)) list (* (pos, name, value) of opt. *)
  | P of string list

  type t = arg Amap.t  (* command line, maps arg_infos to arg value. *)

  let get_arg cl a = try Amap.find a cl with Not_found -> assert false
  let opt_arg cl a = match get_arg cl a with O l -> l | _ -> assert false
  let pos_arg cl a = match get_arg cl a with P l -> l | _ -> assert false

  let arg_info_indexes al =
    (* from [al] returns a trie mapping the names of optional arguments to
       their arg_info, a list with all arg_info for positional arguments and
       a cmdline mapping each arg_info to an empty [arg]. *)
    let rec loop optidx posidx cl = function
    | [] -> optidx, posidx, cl
    | a :: l ->
        match Cmdliner_info.arg_is_pos a with
        | true -> loop optidx (a :: posidx) (Amap.add a (P []) cl) l
        | false ->
            let add t name = Cmdliner_trie.add t name a in
            let names = Cmdliner_info.arg_opt_names a in
            let optidx = List.fold_left add optidx names in
            loop optidx posidx (Amap.add a (O []) cl) l
    in
    loop Cmdliner_trie.empty [] Amap.empty al

  (* Optional argument parsing *)

  let is_opt s = String.length s > 1 && s.[0] = '-'
  let is_short_opt s = String.length s = 2 && s.[0] = '-'

  let parse_opt_arg s = (* (name, value) of opt arg, assert len > 1. *)
    let l = String.length s in
    if s.[1] <> '-' then (* short opt *)
      if l = 2 then s, None else
      String.sub s 0 2, Some (String.sub s 2 (l - 2)) (* with glued opt arg *)
    else try (* long opt *)
      let i = String.index s '=' in
      String.sub s 0 i, Some (String.sub s (i + 1) (l - i - 1))
    with Not_found -> s, None

  let hint_matching_opt optidx s =
    (* hint options that could match [s] in [optidx]. FIXME this is
       a bit obscure. *)
    if String.length s <= 2 then [] else
    let short_opt, long_opt =
      if s.[1] <> '-'
      then s, Printf.sprintf "-%s" s
      else String.sub s 1 (String.length s - 1), s
    in
    let short_opt, _ = parse_opt_arg short_opt in
    let long_opt, _ = parse_opt_arg long_opt in
    let all = Cmdliner_trie.ambiguities optidx "-" in
    match List.mem short_opt all, Cmdliner_suggest.value long_opt all with
    | false, [] -> []
    | false, l -> l
    | true, [] -> [short_opt]
    | true, l -> if List.mem short_opt l then l else short_opt :: l

  let parse_opt_args ~peek_opts optidx cl args =
    (* returns an updated [cl] cmdline according to the options found in [args]
       with the trie index [optidx]. Positional arguments are returned in order
       in a list. *)
    let rec loop errs k cl pargs = function
    | [] -> List.rev errs, cl, List.rev pargs
    | "--" :: args -> List.rev errs, cl, (List.rev_append pargs args)
    | s :: args ->
        if not (is_opt s) then loop errs (k + 1) cl (s :: pargs) args else
        let name, value = parse_opt_arg s in
        match Cmdliner_trie.find optidx name with
        | `Ok a ->
            let value, args = match value, Cmdliner_info.arg_opt_kind a with
            | Some v, Cmdliner_info.Flag when is_short_opt name ->
                None, ("-" ^ v) :: args
            | Some _, _ -> value, args
            | None, Cmdliner_info.Flag -> value, args
            | None, _ ->
                match args with
                | [] -> None, args
                | v :: rest -> if is_opt v then None, args else Some v, rest
            in
            let arg = O ((k, name, value) :: opt_arg cl a) in
            loop errs (k + 1) (Amap.add a arg cl) pargs args
        | `Not_found when peek_opts -> loop errs (k + 1) cl pargs args
        | `Not_found ->
            let hints = hint_matching_opt optidx s in
            let err = Cmdliner_base.err_unknown ~kind:"option" ~hints name in
            loop (err :: errs) (k + 1) cl pargs args
        | `Ambiguous ->
            let ambs = Cmdliner_trie.ambiguities optidx name in
            let ambs = List.sort compare ambs in
            let err = Cmdliner_base.err_ambiguous "option" name ambs in
            loop (err :: errs) (k + 1) cl pargs args
    in
    let errs, cl, pargs = loop [] 0 cl [] args in
    if errs = [] then Ok (cl, pargs) else
    let err = String.concat "\n" errs in
    Error (err, cl, pargs)

  let take_range start stop l =
    let rec loop i acc = function
    | [] -> List.rev acc
    | v :: vs ->
        if i < start then loop (i + 1) acc vs else
        if i <= stop then loop (i + 1) (v :: acc) vs else
        List.rev acc
    in
    loop 0 [] l

  let process_pos_args posidx cl pargs =
    (* returns an updated [cl] cmdline in which each positional arg mentioned
       in the list index posidx, is given a value according the list
       of positional arguments values [pargs]. *)
    if pargs = [] then
      let misses = List.filter Cmdliner_info.arg_is_req posidx in
      if misses = [] then Ok cl else
      Error (Cmdliner_msg.err_pos_misses misses, cl)
    else
    let last = List.length pargs - 1 in
    let pos rev k = if rev then last - k else k in
    let rec loop misses cl max_spec = function
    | [] -> misses, cl, max_spec
    | a :: al ->
        let apos = Cmdliner_info.arg_pos a in
        let rev = Cmdliner_info.pos_rev apos in
        let start = pos rev (Cmdliner_info.pos_start apos) in
        let stop = match Cmdliner_info.pos_len apos with
        | None -> pos rev last
        | Some n -> pos rev (Cmdliner_info.pos_start apos + n - 1)
        in
        let start, stop = if rev then stop, start else start, stop in
        let args = take_range start stop pargs in
        let max_spec = max stop max_spec in
        let cl = Amap.add a (P args) cl in
        let misses = match Cmdliner_info.arg_is_req a && args = [] with
        | true -> (a:: misses)
        | false -> misses
        in
        loop misses cl max_spec al
    in
    let misses, cl, max_spec = loop [] cl (-1) posidx in
    if misses <> [] then Error (Cmdliner_msg.err_pos_misses misses, cl) else
    if last <= max_spec then Ok cl else
    let excess = take_range (max_spec + 1) last pargs in
    Error (Cmdliner_msg.err_pos_excess excess, cl)

  let create ?(peek_opts = false) al args =
    let optidx, posidx, cl = arg_info_indexes al in
    match parse_opt_args ~peek_opts optidx cl args with
    | Ok (cl, _) when peek_opts -> Ok cl
    | Ok (cl, pargs) -> process_pos_args posidx cl pargs
    | Error (errs, cl, _) -> Error (errs, cl)
end

type term_escape =
  [ `Error of bool * string
  | `Help of Manpage.format * string option ]

type 'a term =
  Cmdliner_info.arg list *
  (Cmdliner_info.eval -> Cmdline.t ->
   ('a, [ `Parse of string | term_escape ]) result)

module Arg = struct

  let err_not_opt = "Option argument without name"
  let err_not_pos = "Positional argument with a name"

  let rev_compare n0 n1 = compare n1 n0
  let str_of_pp pp v = pp Format.str_formatter v; Format.flush_str_formatter ()

  type 'a parser = string -> [ `Ok of 'a | `Error of string ]
  type 'a printer = Format.formatter -> 'a -> unit
  type 'a converter = 'a parser * 'a printer
  type env = Cmdliner_info.env

  type 'a t = 'a term
  type info = Cmdliner_info.arg



  let env_var = Cmdliner_info.env

  let ( & ) f x = f x

  let info = Cmdliner_info.arg

  let err e = Error (`Parse e)

  let parse_to_list parser s = match parser s with
  | `Ok v -> `Ok [v]
  | `Error _ as e -> e

  let try_env ei a parse ~absent = match Cmdliner_info.arg_env a with
  | None -> Ok absent
  | Some env ->
      let var = Cmdliner_info.env_var env in
      match Cmdliner_info.(eval_env_var ei var) with
      | None -> Ok absent
      | Some v ->
          match parse v with
          | `Ok v -> Ok v
          | `Error e -> err (Cmdliner_msg.err_env_parse env ~err:e)

  let flag a =
    if Cmdliner_info.arg_is_pos a then invalid_arg err_not_opt else
    let convert ei cl = match Cmdline.opt_arg cl a with
    | [] -> try_env ei a Cmdliner_base.env_bool_parse ~absent:false
    | [_, _, None] -> Ok true
    | [_, f, Some v] -> err (Cmdliner_msg.err_flag_value f v)
    | (_, f, _) :: (_ ,g, _) :: _  -> err (Cmdliner_msg.err_opt_repeated f g)
    in
    [a], convert

  let flag_all a =
    if Cmdliner_info.arg_is_pos a then invalid_arg err_not_opt else
    let a = Cmdliner_info.arg_make_all_opts a in
    let convert ei cl = match Cmdline.opt_arg cl a with
    | [] ->
        try_env ei a (parse_to_list Cmdliner_base.env_bool_parse) ~absent:[]
    | l ->
        try
          let truth (_, f, v) = match v with
          | None -> true
          | Some v -> failwith (Cmdliner_msg.err_flag_value f v)
          in
          Ok (List.rev_map truth l)
        with Failure e -> err e
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
              | Some (g, _) -> failwith (Cmdliner_msg.err_opt_repeated g f)
              end
          | [_, f, Some v] -> failwith (Cmdliner_msg.err_flag_value f v)
          | (_, f, _) :: (_, g, _) :: _ ->
              failwith (Cmdliner_msg.err_opt_repeated g f)
          end
      | [] -> match fv with None -> v | Some (_, v) -> v
      in
      try Ok (aux None l) with Failure e -> err e
    in
    let flag (_, a) =
      if Cmdliner_info.arg_is_pos a then invalid_arg err_not_opt else a
    in
    List.rev_map flag l, convert

  let vflag_all v l =
    let convert _ cl =
      let rec aux acc = function
      | (fv, a) :: rest ->
          begin match Cmdline.opt_arg cl a with
          | [] -> aux acc rest
          | l ->
              let fval (k, f, v) = match v with
              | None -> (k, fv)
              | Some v -> failwith (Cmdliner_msg.err_flag_value f v)
              in
              aux (List.rev_append (List.rev_map fval l) acc) rest
          end
      | [] ->
          if acc = [] then v else List.rev_map snd (List.sort rev_compare acc)
      in
      try Ok (aux [] l) with Failure e -> err e
    in
    let flag (_, a) =
      if Cmdliner_info.arg_is_pos a then invalid_arg err_not_opt else
      Cmdliner_info.arg_make_all_opts a
    in
    List.rev_map flag l, convert

  let parse_opt_value parse f v = match parse v with
  | `Ok v -> v
  | `Error e -> failwith (Cmdliner_msg.err_opt_parse f e)

  let opt ?vopt (parse, print) v a =
    if Cmdliner_info.arg_is_pos a then invalid_arg err_not_opt else
    let absent = Cmdliner_info.Val (lazy (str_of_pp print v)) in
    let kind = match vopt with
    | None -> Cmdliner_info.Opt
    | Some dv -> Cmdliner_info.Opt_vopt (str_of_pp print dv)
    in
    let a = Cmdliner_info.arg_make_opt ~absent ~kind a in
    let convert ei cl = match Cmdline.opt_arg cl a with
    | [] -> try_env ei a parse ~absent:v
    | [_, f, Some v] ->
        (try Ok (parse_opt_value parse f v) with Failure e -> err e)
    | [_, f, None] ->
        begin match vopt with
        | None -> err (Cmdliner_msg.err_opt_value_missing f)
        | Some optv -> Ok optv
        end
    | (_, f, _) :: (_, g, _) :: _ -> err (Cmdliner_msg.err_opt_repeated g f)
    in
    [a], convert

  let opt_all ?vopt (parse, print) v a =
    if Cmdliner_info.arg_is_pos a then invalid_arg err_not_opt else
    let absent = Cmdliner_info.Val (lazy "") in
    let kind = match vopt with
    | None -> Cmdliner_info.Opt
    | Some dv -> Cmdliner_info.Opt_vopt (str_of_pp print dv)
    in
    let a = Cmdliner_info.arg_make_opt_all ~absent ~kind a in
    let convert ei cl = match Cmdline.opt_arg cl a with
    | [] -> try_env ei a (parse_to_list parse) ~absent:v
    | l ->
        let parse (k, f, v) = match v with
        | Some v -> (k, parse_opt_value parse f v)
        | None -> match vopt with
        | None -> failwith (Cmdliner_msg.err_opt_value_missing f)
        | Some dv -> (k, dv)
        in
        try Ok (List.rev_map snd
                  (List.sort rev_compare (List.rev_map parse l))) with
        | Failure e -> err e
    in
    [a], convert

  (* Positional arguments *)

  let parse_pos_value parse a v = match parse v with
  | `Ok v -> v
  | `Error e -> failwith (Cmdliner_msg.err_pos_parse a e)

  let pos ?(rev = false) k (parse, print) v a =
    if Cmdliner_info.arg_is_opt a then invalid_arg err_not_pos else
    let absent = Cmdliner_info.Val (lazy (str_of_pp print v)) in
    let pos = Cmdliner_info.pos ~rev ~start:k ~len:(Some 1) in
    let a = Cmdliner_info.arg_make_pos_abs ~absent ~pos a in
    let convert ei cl = match Cmdline.pos_arg cl a with
    | [] -> try_env ei a parse ~absent:v
    | [v] ->
        (try Ok (parse_pos_value parse a v) with Failure e -> err e)
    | _ -> assert false
    in
    [a], convert

  let pos_list pos (parse, _) v a =
    if Cmdliner_info.arg_is_opt a then invalid_arg err_not_pos else
    let a = Cmdliner_info.arg_make_pos pos a in
    let convert ei cl = match Cmdline.pos_arg cl a with
    | [] -> try_env ei a (parse_to_list parse) ~absent:v
    | l ->
        try Ok (List.rev (List.rev_map (parse_pos_value parse a) l)) with
        | Failure e -> err e
    in
    [a], convert

  let all = Cmdliner_info.pos ~rev:false ~start:0 ~len:None
  let pos_all c v a = pos_list all c v a

  let pos_left ?(rev = false) k =
    let start = if rev then k + 1 else 0 in
    let len = if rev then None else Some k in
    pos_list (Cmdliner_info.pos ~rev ~start ~len)

  let pos_right ?(rev = false) k =
    let start = if rev then 0 else k + 1 in
    let len = if rev then Some k else None in
    pos_list (Cmdliner_info.pos ~rev ~start ~len)

  (* Arguments as terms *)

  let absent_error al = List.rev_map Cmdliner_info.arg_make_req al
  let value a = a

  let required (al, convert) =
    let al = absent_error al in
    let convert ei cl = match convert ei cl with
    | Ok (Some v) -> Ok v
    | Ok None -> err (Cmdliner_msg.err_arg_missing (List.hd al))
    | Error _ as e -> e
    in
    al, convert

  let non_empty (al, convert) =
    let al = absent_error al in
    let convert ei cl = match convert ei cl with
    | Ok [] -> err (Cmdliner_msg.err_arg_missing (List.hd al))
    | Ok l -> Ok l
    | Error _ as e -> e
    in
    al, convert

  let last (al, convert) =
    let convert ei cl = match convert ei cl with
    | Ok [] -> err (Cmdliner_msg.err_arg_missing (List.hd al))
    | Ok l -> Ok (List.hd (List.rev l))
    | Error _ as e -> e
    in
    al, convert

  (* Predefined converters. *)

  let some = Cmdliner_base.some
  let bool = Cmdliner_base.bool
  let char = Cmdliner_base.char
  let int = Cmdliner_base.int
  let nativeint = Cmdliner_base.nativeint
  let int32 = Cmdliner_base.int32
  let int64 = Cmdliner_base.int64
  let float = Cmdliner_base.float
  let string = Cmdliner_base.string
  let enum = Cmdliner_base.enum
  let file = Cmdliner_base.file
  let dir = Cmdliner_base.dir
  let non_dir_file = Cmdliner_base.non_dir_file
  let list = Cmdliner_base.list
  let array = Cmdliner_base.array
  let pair = Cmdliner_base.pair
  let t2 = Cmdliner_base.t2
  let t3 = Cmdliner_base.t3
  let t4 = Cmdliner_base.t4

  (* Documentation formatting helpers *)

  let doc_quote = Cmdliner_base.quote
  let doc_alts = Cmdliner_base.alts_str
  let doc_alts_enum ?quoted enum = doc_alts ?quoted (List.map fst enum)
end

module Stdopts = struct

  let strf = Printf.sprintf

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

  let add ei =
    let docs = Cmdliner_info.(term_stdopts_docs @@ eval_term ei) in
    let args, v_lookup =
      if Cmdliner_info.(term_version @@ eval_main ei) = None then [], None else
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
    let term = Cmdliner_info.(term_add_args (eval_term ei) args) in
    h_lookup, v_lookup, Cmdliner_info.eval_with_term ei term
end

module Term = struct

  let err_help s = "Term error, help requested for unknown command " ^ s
  let err_argv = "argv array must have at least one element"

  type 'a ret = [ `Ok of 'a | term_escape ]
  type +'a t = 'a term

  type 'a result =
    [ `Ok of 'a | `Error of [`Parse | `Term | `Exn ] | `Version | `Help ]

  let const v = [], (fun _ _ -> Ok v)
  let pure (* deprecated *) = const
  let app (al, f) (al', v) =
    List.rev_append al al',
    fun ei cl -> match (f ei cl) with
    | Error _ as e -> e
    | Ok f ->
        match v ei cl with
        | Error _ as e -> e
        | Ok v -> Ok (f v)

  let ( $ ) = app

  let ret (al, v) =
    al, fun ei cl -> match v ei cl with
    | Ok (`Ok v) -> Ok v
    | Ok (`Error _ as err) -> Error err
    | Ok (`Help _ as help) -> Error help
    | Error _ as e -> e

  let main_name =
    [], (fun ei _ -> Ok (Cmdliner_info.(term_name @@ eval_main ei)))

  let choice_names =
    let choice_name t = Cmdliner_info.term_name t in
    [],
    fun ei _ -> Ok (List.rev_map choice_name (Cmdliner_info.eval_choices ei))

  let man_format = Stdopts.man_format

  (* Term information *)

  type info = Cmdliner_info.term
  let info = Cmdliner_info.term ~args:[]
  let name ti = Cmdliner_info.term_name ti

  (* Evaluation *)

  let remove_exec argv =
    try List.tl (Array.to_list argv) with Failure _ -> invalid_arg err_argv

  let eval_help_cmd help ei fmt cmd =
    let ei = match cmd with
    | None -> Cmdliner_info.(eval_with_term ei @@ eval_main ei)
    | Some cmd ->
        try
          let is_cmd t = Cmdliner_info.term_name t = cmd in
          let cmd = List.find is_cmd (Cmdliner_info.eval_choices ei) in
          Cmdliner_info.eval_with_term ei cmd
        with Not_found -> invalid_arg (err_help cmd)
    in
    let _, _, ei = Stdopts.add ei in
    Cmdliner_docgen.pp_man fmt help ei; `Help

  let eval_err help_ppf err_ppf ei = function
  | `Help (fmt, cmd) -> eval_help_cmd help_ppf ei fmt cmd
  | `Parse err -> Cmdliner_msg.pp_err_usage err_ppf ei ~err; `Error `Parse
  | `Error (usage, err) ->
      (if usage
       then Cmdliner_msg.pp_err_usage err_ppf ei ~err
       else Cmdliner_msg.pp_err err_ppf ei ~err);
      `Error `Term

  let eval_fun ~catch help_ppf err_ppf ei cl f =
    try match f ei cl with
    | Error err -> eval_err help_ppf err_ppf ei err
    | Ok v -> `Ok v
    with
    | exn when catch ->
        let bt = Printexc.get_backtrace () in
        Cmdliner_msg.pp_backtrace err_ppf ei exn bt; `Error `Exn

  let eval_term ~catch help_ppf err_ppf ei f args =
    let help_arg, vers_arg, ei = Stdopts.add ei in
    let term_args = Cmdliner_info.(term_args @@ eval_term ei) in
    match Cmdline.create term_args args with
    | Error (e, cl) ->
        begin match help_arg ei cl with
        | Error err -> eval_err help_ppf err_ppf ei err
        | Ok (Some fmt) -> Cmdliner_docgen.pp_man fmt help_ppf ei; `Help
        | Ok None -> eval_err help_ppf err_ppf ei (`Error (true, e))
        end
    | Ok cl ->
        match help_arg ei cl with
        | Error err -> eval_err help_ppf err_ppf ei err
        | Ok (Some fmt) -> Cmdliner_docgen.pp_man fmt help_ppf ei; `Help
        | Ok None ->
            match vers_arg with
            | None -> eval_fun ~catch help_ppf err_ppf ei cl f
            | Some v_arg ->
                match v_arg ei cl with
                | Error err -> eval_err help_ppf err_ppf ei err
                | Ok true -> Cmdliner_msg.pp_version help_ppf ei; `Version
                | Ok false -> eval_fun ~catch help_ppf err_ppf ei cl f

  let term_eval_peek_opts ei f args =
    let ret_to_opt = function
    | `Ok v -> Some v | `Error _ -> None | `Help -> None
    in
    let eval_err = function
    | `Help _ -> `Help
    | `Parse _ -> `Error `Parse
    | `Error _ -> `Error `Term
    in
    let eval_fun ei cl f =
      try match f ei cl with
      | Ok v -> `Ok v
      | Error err -> eval_err err
      with e -> `Error `Exn
    in
    let help_arg, vers_arg, ei = Stdopts.add ei in
    let term_args = Cmdliner_info.(term_args @@ eval_term ei) in
    match Cmdline.create ~peek_opts:true term_args args with
    | Error (e, cl) ->
        begin match help_arg ei cl with
        | Ok (Some fmt) -> None, `Help
        | Ok None -> None, eval_err (`Parse e)
        | Error err -> None, eval_err err
        end
    | Ok cl ->
        let ret = eval_fun ei cl f in
        match help_arg ei cl with
        | Error err -> None, eval_err err
        | Ok (Some _) -> (ret_to_opt @@ ret), `Help
        | Ok None ->
            match vers_arg with
            | None -> (ret_to_opt @@ ret), ret
            | Some varg ->
                match varg ei cl with
                | Error _ -> None, (`Error `Parse)
                | Ok true -> (ret_to_opt @@ ret), `Version
                | Ok false -> (ret_to_opt @@ ret), ret

  let env_default v = try Some (Sys.getenv v) with Not_found -> None

  let eval
      ?help:(help_ppf = Format.std_formatter)
      ?err:(err_ppf = Format.err_formatter)
      ?(catch = true) ?(env = env_default) ?(argv = Sys.argv) ((al, f), ti) =
    let term = Cmdliner_info.term_add_args ti al in
    let ei = Cmdliner_info.eval ~term ~main:term ~choices:[] ~env in
    eval_term catch help_ppf err_ppf ei f (remove_exec argv)

  let choose_term main choices = function
  | [] -> Ok (main, [])
  | maybe :: args' as args ->
      if String.length maybe > 1 && maybe.[0] = '-' then Ok (main, args) else
      let index =
        let add acc (choice, _ as c) =
          let name = Cmdliner_info.term_name choice in
          Cmdliner_trie.add acc name c
        in
        List.fold_left add Cmdliner_trie.empty choices
      in
      match Cmdliner_trie.find index maybe with
      | `Ok choice -> Ok (choice, args')
      | `Not_found ->
          let all = Cmdliner_trie.ambiguities index "" in
          let hints = Cmdliner_suggest.value maybe all in
          Error (Cmdliner_base.err_unknown ~kind:"command" maybe ~hints)
      | `Ambiguous ->
          let ambs = Cmdliner_trie.ambiguities index maybe in
          let ambs = List.sort compare ambs in
          Error (Cmdliner_base.err_ambiguous ~kind:"command" maybe ~ambs)

  let eval_choice
      ?help:(help_ppf = Format.std_formatter)
      ?err:(err_ppf = Format.err_formatter)
      ?(catch = true) ?(env = env_default) ?(argv = Sys.argv)
      main choices =
    let to_term_f ((al, f), ti) = Cmdliner_info.term_add_args ti al, f in
    let choices_f = List.rev_map to_term_f choices in
    let main_f = to_term_f main in
    let choices = List.rev_map fst choices_f in
    let main = fst main_f in
    match choose_term main_f choices_f (remove_exec argv) with
    | Error err ->
        let ei = Cmdliner_info.eval ~term:main ~main ~choices ~env in
        Cmdliner_msg.pp_err_usage err_ppf ei ~err; `Error `Parse
    | Ok ((chosen, f), args) ->
        let ei = Cmdliner_info.eval ~term:chosen ~main ~choices ~env in
        eval_term catch help_ppf err_ppf ei f args

  let eval_peek_opts
      ?(version_opt = false) ?(env = env_default) ?(argv = Sys.argv)
      ((args, f) : 'a t) =
    let version = if version_opt then Some "dummy" else None in
    let term = Cmdliner_info.term ~args ?version "dummy" in
    let ei = Cmdliner_info.eval ~term ~main:term ~choices:[] ~env  in
    (term_eval_peek_opts ei f (remove_exec argv) :> 'a option * 'a result)
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
