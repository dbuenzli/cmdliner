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

(* Formatting tools *)

module Fmt = struct
  type 'a t = Format.formatter -> 'a -> unit
  let pf = Format.fprintf
  let sp = Format.pp_print_space
  let cut = Format.pp_print_cut
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

  let some ?(none = "") conv =
    let parser s = match parser conv s with
    | Ok v -> Ok (Some v) | Error _ as e -> e
    in
    let pp ppf v = match v with
    | None -> Format.pp_print_string ppf none
    | Some v -> pp conv ppf v
    in
    let completion = Completion.some (completion conv) in
    { conv with parser; pp; completion }

  let some' ?none conv =
    let parser s = match parser conv s with
    | Ok v -> Ok (Some v) | Error _ as e -> e
    in
    let pp ppf = function
    | None -> (match none with None -> () | Some v -> (pp conv) ppf v)
    | Some v -> pp conv ppf v
    in
    let completion = Completion.some conv.completion in
    { conv with parser; pp; completion }
end
