(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

(* Unique ids *)

let uid =
  (* Thread-safe UIDs, Oo.id (object end) was used before.
     Note this won't be thread-safe in multicore, we should use
     Atomic but this is >= 4.12 and we have 4.08 for now. *)
  let c = ref 0 in
  fun () ->
    let id = !c in
    incr c; if id > !c then assert false (* too many ids *) else id

(* Edit distance

   The stdlib has much better in but this will be only >= 5.4, maybe
   in twenty years. *)

let edit_distance s0 s1 =
  let minimum (a : int) (b : int) (c : int) : int = min a (min b c) in
  let s0,s1 = if String.length s0 <= String.length s1 then s0,s1 else s1,s0 in
  let m = String.length s0 and n = String.length s1 in
  let rec rows row0 row i = match i > n with
  | true -> row0.(m)
  | false ->
      row.(0) <- i;
      for j = 1 to m do
        if s0.[j - 1] = s1.[i - 1] then row.(j) <- row0.(j - 1) else
        row.(j) <- minimum (row0.(j - 1) + 1) (row0.(j) + 1) (row.(j - 1) + 1)
      done;
      rows row row0 (i + 1)
  in
  rows (Array.init (m + 1) (fun x -> x)) (Array.make (m + 1) 0) 1

let suggest s candidates =
  let add (min, acc) name =
    let d = edit_distance s name in
    if d = min then min, (name :: acc) else
    if d < min then d, [name] else
    min, acc
  in
  let dist, suggs = List.fold_left add (max_int, []) candidates in
  if dist < 3 (* suggest only if not too far *) then suggs else []

(* Stdlib compatibility *)

let is_space = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false

let string_has_prefix ~prefix s =
  let prefix_len = String.length prefix in
  let s_len = String.length s in
  if prefix_len > s_len then false else
  let rec loop i =
    if i = prefix_len then true
    else if String.get prefix i = String.get s i then loop (i + 1)
    else false
  in
  loop 0

let string_drop_prefix ~prefix s =
  if string_has_prefix ~prefix s then
    let drop = String.length prefix in
    Some (String.sub s drop (String.length s - drop))
  else None

(* Invalid argument strings *)

let err_empty_list = "empty list"
let err_incomplete_enum ss =
  strf "Arg.enum: missing printable string for a value, other strings are: %s"
    (String.concat ", " ss)

(* Formatting tools *)

module Fmt = struct
  type 'a t = Format.formatter -> 'a -> unit
  let pf = Format.fprintf
  let sp = Format.pp_print_space
  let string = Format.pp_print_string
  let char = Format.pp_print_char
  let indent ppf c = for i = 1 to c do char ppf ' ' done
  let text = Format.pp_print_text
  let lines ppf s =
    let rec stop_at sat ~start ~max s =
      if start > max then start else
      if sat s.[start] then start else
      stop_at sat ~start:(start + 1) ~max s
    in
    let sub s start stop ~max =
      if start = stop then "" else
      if start = 0 && stop > max then s else
      String.sub s start (stop - start)
    in
    let is_nl c = c = '\n' in
    let max = String.length s - 1 in
    let rec loop start s = match stop_at is_nl ~start ~max s with
    | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
    | stop ->
        Format.pp_print_string ppf (sub s start stop ~max);
        Format.pp_force_newline ppf ();
        loop (stop + 1) s
    in
    loop 0 s

  let tokens ~spaces ppf s = (* collapse white and hint spaces (maybe) *)
    let i_max = String.length s - 1 in
    let flush start stop = string ppf (String.sub s start (stop - start + 1)) in
    let rec skip_white i =
      if i > i_max then i else
      if is_space s.[i] then skip_white (i + 1) else i
    in
    let rec loop start i =
      if i > i_max then flush start i_max else
      if not (is_space s.[i]) then loop start (i + 1) else
      let next_start = skip_white i in
      (flush start (i - 1); if spaces then sp ppf () else char ppf ' ';
       if next_start > i_max then () else loop next_start next_start)
    in
    loop 0 0
end

(* Converter (end-user) error messages *)

let quote s = strf "'%s'" s
let alts_str ?quoted alts =
  let quote = match quoted with
  | None -> strf "$(b,%s)"
  | Some quoted -> if quoted then quote else (fun s -> s)
  in
  match alts with
  | [] -> invalid_arg err_empty_list
  | [a] -> (quote a)
  | [a; b] -> strf "either %s or %s" (quote a) (quote b)
  | alts ->
      let rev_alts = List.rev alts in
      strf "one of %s or %s"
        (String.concat ", " (List.rev_map quote (List.tl rev_alts)))
        (quote (List.hd rev_alts))

let err_multi_def ~kind name doc v v' =
  strf "%s %s defined twice (doc strings are '%s' and '%s')"
    kind name (doc v) (doc v')

let err_ambiguous ~kind s ~ambs =
  strf "%s %s ambiguous and could be %s" kind (quote s)
    (alts_str ~quoted:true ambs)

let err_unknown ?(dom = []) ?(hints = []) ~kind v =
  let hints = match hints, dom with
  | [], [] -> "."
  | [], dom -> strf ", must be %s." (alts_str ~quoted:true dom)
  | hints, _ -> strf ", did you mean %s?" (alts_str ~quoted:true hints)
  in
  strf "unknown %s %s%s" kind (quote v) hints

let err_no kind s = strf "no %s %s" (quote s) kind
let err_not_dir s = strf "%s is not a directory" (quote s)
let err_is_dir s = strf "%s is a directory" (quote s)
let err_element kind s exp =
  strf "invalid element in %s ('%s'): %s" kind s exp

let err_invalid kind s exp = strf "invalid %s %s, %s" kind (quote s) exp
let err_invalid_val = err_invalid "value"
let err_sep_miss sep s =
  err_invalid_val s (strf "missing a '%c' separator" sep)

(* Completions *)

module Completion = struct
  type complete = string -> (string * string) list
  type 'a t =
    { files : bool;
      dirs : bool;
      complete : complete; }

  let make ?(files = false) ?(dirs = false) ?(complete = Fun.const []) () =
    {files; dirs; complete}

  let none = make ()
  let files c = c.files
  let dirs c = c.dirs
  let complete c = c.complete
  let some = (Fun.id :> 'a t -> 'a option t)
end

(* Converters *)

module Conv = struct
  type 'a parser = string -> ('a, string) result
  type 'a fmt = Format.formatter -> 'a -> unit
  type 'a t =
    { docv : string;
      parser : 'a parser;
      pp : 'a fmt;
      completion : 'a Completion.t; }

  let make ?(completion = Completion.none) ~docv ~parser ~pp () =
    { docv; parser; pp; completion }

  let of_conv
      conv ?(completion = conv.completion) ?(docv = conv.docv)
      ?(parser = conv.parser) ?(pp = conv.pp) ()
    =
    { docv; parser; pp; completion }

  let docv c = c.docv
  let parser c = c.parser
  let pp c = c.pp
  let completion c = c.completion
end

let some ?(none = "") conv =
  let parser s = match Conv.parser conv s with
  | Ok v -> Ok (Some v) | Error _ as e -> e
  in
  let pp ppf v = match v with
  | None -> Format.pp_print_string ppf none
  | Some v -> Conv.pp conv ppf v
  in
  { conv with parser; pp; completion = Completion.some conv.completion }

let some' ?none conv =
  let parser s = match (Conv.parser conv) s with
  | Ok v -> Ok (Some v) | Error _ as e -> e
  in
  let pp ppf = function
  | None -> (match none with None -> () | Some v -> (Conv.pp conv) ppf v)
  | Some v -> Conv.pp conv ppf v
  in
  { conv with parser; pp; completion = Completion.some conv.completion }

let bool =
  let parser s = try Ok (bool_of_string s) with
  | Invalid_argument _ ->
      Error (err_invalid_val s (alts_str ~quoted:true ["true"; "false"]))
  in
  Conv.make ~docv:"BOOL" ~parser ~pp:Format.pp_print_bool ()

let char =
  let parser s = match String.length s = 1 with
  | true -> Ok s.[0]
  | false -> Error (err_invalid_val s "expected a character")
  in
  Conv.make ~docv:"CHAR" ~parser ~pp:Fmt.char ()

let parse_with t_of_str exp s =
  try Ok (t_of_str s) with Failure _ -> Error (err_invalid_val s exp)

let int =
  let parser = parse_with int_of_string "expected an integer" in
  Conv.make ~docv:"INT" ~parser ~pp:Format.pp_print_int ()

let int32 =
  let parser = parse_with Int32.of_string "expected a 32-bit integer" in
  let pp ppf = Fmt.pf ppf "%ld" in
  Conv.make ~docv:"INT32" ~parser ~pp ()

let int64 =
  let parser = parse_with Int64.of_string "expected a 64-bit integer" in
  let pp ppf = Fmt.pf ppf "%Ld" in
  Conv.make ~docv:"INT64" ~parser ~pp ()

let nativeint =
  let err = "expected a processor-native integer" in
  let parser = parse_with Nativeint.of_string err in
  let pp ppf = Fmt.pf ppf "%nd" in
  Conv.make ~docv:"NATIVEINT" ~parser ~pp ()

let float =
  let parser = parse_with float_of_string "expected a floating point number" in
  Conv.make ~docv:"DOUBLE" ~parser ~pp:Format.pp_print_float ()

let string = Conv.make ~docv:"" ~parser:Result.ok ~pp:Fmt.string ()

let enum sl =
  if sl = [] then invalid_arg err_empty_list else
  let t = Cmdliner_trie.of_list sl in
  let parser s = match Cmdliner_trie.find t s with
  | `Ok v -> Ok v
  | `Ambiguous ->
      let ambs = List.sort compare (Cmdliner_trie.ambiguities t s) in
      Error (err_ambiguous ~kind:"enum value" s ~ambs)
  | `Not_found ->
        let alts = List.rev (List.rev_map (fun (s, _) -> s) sl) in
        Error (err_invalid_val s ("expected " ^ (alts_str ~quoted:true alts)))
  in
  let pp ppf v =
    let sl_inv = List.rev_map (fun (s,v) -> (v,s)) sl in
    try Fmt.string ppf (List.assoc v sl_inv)
    with Not_found -> invalid_arg (err_incomplete_enum (List.map fst sl))
  in
  let complete _prefix = List.map (fun (s, _) -> s, "") sl in
  let completion = Completion.make ~complete () in
  Conv.make ~docv:"ENUM" ~parser ~pp ~completion ()

let file =
  let parser s =
    if s = "-" then Ok s else
    if Sys.file_exists s then Ok s else
    Error (err_no "file or directory" s)
  in
  let completion = Completion.make ~dirs:true ~files:true () in
  Conv.make ~docv:"PATH" ~parser ~pp:Fmt.string ~completion ()

let dir =
  let parser s =
    if Sys.file_exists s
    then (if Sys.is_directory s then Ok s else Error (err_not_dir s))
    else Error (err_no "directory" s)
  in
  let completion = Completion.make ~dirs:true () in
  Conv.make ~docv:"DIR" ~parser ~pp:Fmt.string ~completion ()

let non_dir_file =
  let parser s =
    if s = "-" then Ok s else
    if Sys.file_exists s
    then (if not (Sys.is_directory s) then Ok s else Error (err_is_dir s))
    else Error (err_no "file" s)
  in
  let completion = Completion.make ~files:true () in
  Conv.make ~docv:"FILE" ~parser ~pp:Fmt.string ~completion ()

let split_and_parse sep parse s = (* raises [Failure] *)
  let parse sub = match parse sub with
  | Error e -> failwith e | Ok v -> v
  in
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

let list ?(sep = ',') conv =
  let parser s = try Ok (split_and_parse sep (Conv.parser conv) s) with
  | Failure e -> Error (err_element "list" s e)
  in
  let rec pp ppf = function
  | [] -> ()
  | v :: l ->
      (Conv.pp conv) ppf v; if (l <> []) then (Fmt.char ppf sep; pp ppf l)
  in
  let docv = strf "%s[%c…]" conv.docv sep in
  Conv.make ~docv ~parser ~pp ()

let array ?(sep = ',') conv =
  let parser s =
    try Ok (Array.of_list (split_and_parse sep (Conv.parser conv) s)) with
    | Failure e -> Error (err_element "array" s e)
  in
  let pp ppf v =
    let max = Array.length v - 1 in
    for i = 0 to max do
      conv.pp ppf v.(i); if i <> max then Fmt.char ppf sep
    done
  in
  let docv = strf "%s[%c…]" conv.docv sep in
  Conv.make ~docv ~parser ~pp ()

let split_left sep s =
  try
    let i = String.index s sep in
    let len = String.length s in
    Some ((String.sub s 0 i), (String.sub s (i + 1) (len - i - 1)))
  with Not_found -> None

let pair ?(sep = ',') conv0 conv1 =
  let parser s = match split_left sep s with
  | None -> Error (err_sep_miss sep s)
  | Some (v0, v1) ->
      match (Conv.parser conv0) v0, (Conv.parser conv1) v1 with
      | Ok v0, Ok v1 -> Ok (v0, v1)
      | Error e, _ | _, Error e -> Error (err_element "pair" s e)
  in
  let pp ppf (v0, v1) =
    Fmt.pf ppf "%a%c%a" (Conv.pp conv0) v0 sep (Conv.pp conv1) v1
  in
  let docv = strf "%s%c%s" (Conv.docv conv0) sep (Conv.docv conv1) in
  Conv.make ~docv ~parser ~pp ()

let t2 = pair
let t3 ?(sep = ',') conv0 conv1 conv2 =
  let parser s = match split_left sep s with
  | None -> Error (err_sep_miss sep s)
  | Some (v0, s) ->
      match split_left sep s with
      | None -> Error (err_sep_miss sep s)
      | Some (v1, v2) ->
          match (Conv.parser conv0) v0, (Conv.parser conv1) v1,
                (Conv.parser conv2) v2 with
          | Ok v0, Ok v1, Ok v2 -> Ok (v0, v1, v2)
          | Error e, _, _ | _, Error e, _ | _, _, Error e ->
              Error (err_element "triple" s e)
  in
  let pp ppf (v0, v1, v2) =
    Fmt.pf ppf "%a%c%a%c%a" conv0.pp v0 sep conv1.pp v1 sep conv2.pp v2
  in
  let docv = strf "%s%c%s%c%s" conv0.docv sep conv1.docv sep conv2.docv in
  Conv.make ~docv ~parser ~pp ()

let t4 ?(sep = ',') conv0 conv1 conv2 conv3 =
  let parser s = match split_left sep s with
  | None -> Error (err_sep_miss sep s)
  | Some(v0, s) ->
      match split_left sep s with
      | None -> Error (err_sep_miss sep s)
      | Some (v1, s) ->
          match split_left sep s with
          | None -> Error (err_sep_miss sep s)
          | Some (v2, v3) ->
              match (Conv.parser conv0) v0, (Conv.parser conv1) v1,
                    (Conv.parser conv2) v2, (Conv.parser conv3) v3  with
              | Ok v1, Ok v2, Ok v3, Ok v4 -> Ok (v1, v2, v3, v4)
              | Error e, _, _, _ | _, Error e, _, _ | _, _, Error e, _
              | _, _, _, Error e -> Error (err_element "quadruple" s e)
  in
  let pp ppf (v0, v1, v2, v3) =
    Fmt.pf ppf "%a%c%a%c%a%c%a" conv0.pp v0 sep conv1.pp v1 sep conv2.pp
      v2 sep conv3.pp v3
  in
  let docv =
    strf "%s%c%s%c%s%c%s" conv0.docv sep conv1.docv sep conv2.docv
      sep conv3.docv
  in
  Conv.make ~docv ~parser ~pp ()

let env_bool_parse s = match String.lowercase_ascii s with
| "" | "false" | "no" | "n" | "0" -> Ok false
| "true" | "yes" | "y" | "1" -> Ok true
| s ->
    let alts = alts_str ~quoted:true ["true"; "yes"; "false"; "no" ] in
    Error (err_invalid_val s alts)
