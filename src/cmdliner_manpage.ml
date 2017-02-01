(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Manpages *)

type title = string * int * string * string * string
type block =
  [ `S of string | `P of string | `Pre of string | `I of string * string
  | `Noblank | `Blocks of block list ]

type t = title * block list

(* Standard sections *)

let s_name = "NAME"
let s_synopsis = "SYNOPSIS"
let s_description = "DESCRIPTION"
let s_commands = "COMMANDS"
let s_arguments = "ARGUMENTS"
let s_options = "OPTIONS"
let s_common_options = "COMMON OPTIONS"
let s_environment = "ENVIRONMENT"
let s_files = "FILES"
let s_exit_status = "EXIT STATUS"
let s_examples = "EXAMPLES"
let s_bugs = "BUGS"
let s_authors = "AUTHORS"
let s_see_also = "SEE ALSO"

(* Formatting tools *)

let strf = Printf.sprintf
let pf = Format.fprintf
let pp_sp = Format.pp_print_space
let pp_str = Format.pp_print_string
let pp_char = Format.pp_print_char
let pp_indent ppf c = for i = 1 to c do pp_char ppf ' ' done

let pp_tokens ~spaces ppf s = (* collapse white and hint spaces (maybe) *)
  let is_space = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false in
  let i_max = String.length s - 1 in
  let flush start stop = pp_str ppf (String.sub s start (stop - start + 1)) in
  let rec skip_white i =
    if i > i_max then i else
    if is_space s.[i] then skip_white (i + 1) else i
  in
  let rec loop start i =
    if i > i_max then flush start i_max else
    if not (is_space s.[i]) then loop start (i + 1) else
    let next_start = skip_white i in
    (flush start (i - 1); if spaces then pp_sp ppf () else pp_char ppf ' ';
     if next_start > i_max then () else loop next_start next_start)
  in
  loop 0 0

let pp_white_str ~spaces ppf s =  (* hint spaces (maybe) and new lines. *)
  let left = ref 0 and right = ref 0 and len = String.length s in
  let flush () =
    Format.pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    if s.[!right] = '\n' then (flush (); Format.pp_force_newline ppf ()) else
    if spaces && s.[!right] = ' ' then (flush (); Format.pp_print_space ppf ())
    else incr right;
  done;
  if !left <> len then flush ()

let pp_text = pp_white_str ~spaces:true
let pp_lines = pp_white_str ~spaces:false

(* Cmdliner markup handling *)

let err_unescaped c s = invalid_arg (strf "Unescaped %C in %S" c s)
let err_malformed s = invalid_arg (strf "Malformed $(...) in %S" s)
let err_unclosed s = invalid_arg (strf "Unclosed $(...) in %S" s)
let err_undef id s = invalid_arg (strf "Undefined variable $(%s) in %S" id s)
let err_illegal_esc c s = invalid_arg (strf "Illegal escape char %C in %S" c s)
let err_markup dir s =
  invalid_arg (strf "Unknown cmdliner markup $(%c,...) in %S" dir s)

let is_markup_dir = function 'i' | 'b' -> true | _ -> false
let is_markup_esc = function '$' | '\\' | '(' | ')' -> true | _ -> false
let markup_need_esc = function '\\' | '$' -> true | _ -> false
let markup_text_need_esc = function '\\' | '$' | ')' -> true | _ -> false

let markup_text_escape s =
  let max_i = String.length s - 1 in
  let rec escaped_len i l =
    if i > max_i then l else
    if markup_text_need_esc s.[i] then escaped_len (i + 1) (l + 2) else
    escaped_len (i + 1) (l + 1)
  in
  let escaped_len = escaped_len 0 0 in
  if escaped_len = String.length s then s else
  let b = Bytes.create escaped_len in
  let rec loop i k =
    if i > max_i then Bytes.unsafe_to_string b else
    let c = String.unsafe_get s i in
    if not (markup_text_need_esc c)
    then (Bytes.unsafe_set b k c; loop (i + 1) (k + 1))
    else (Bytes.unsafe_set b k '\\'; Bytes.unsafe_set b (k + 1) c;
          loop (i + 1) (k + 2))
  in
  loop 0 0

let subst_vars b ~subst s =
  let max_i = String.length s - 1 in
  let flush start stop = match start > max_i with
  | true -> ()
  | false -> Buffer.add_substring b s start (stop - start + 1)
  in
  let skip_escape k start i =
    if i > max_i then err_unescaped '\\' s else k start (i + 1)
  in
  let rec skip_markup k start i =
    if i > max_i then err_unclosed s else
    match s.[i] with
    | '\\' -> skip_escape (skip_markup k) start (i + 1)
    | ')' -> k start (i + 1)
    | c -> skip_markup k start (i + 1)
  in
  let rec add_subst start i =
    if i > max_i then err_unclosed s else
    if s.[i] <> ')' then add_subst start (i + 1) else
    let id = String.sub s start (i - start) in
    let next = i + 1 in
    match subst id with
    | None -> err_undef id s
    | Some v -> Buffer.add_string b v; loop next next
  and loop start i =
    if i > max_i then flush start max_i else
    let next = i + 1 in
    match s.[i] with
    | '\\' -> skip_escape loop start next
    | '$' ->
        if next > max_i then err_unescaped '$' s else
        begin match s.[next] with
        | '(' ->
            let min = next + 2 in
            if min > max_i then err_unclosed s else
            begin match s.[min] with
            | ',' -> skip_markup loop start (min + 1)
            | _ ->
                let start_id = next + 1 in
                flush start (i - 1); add_subst start_id start_id
            end
        | _ -> err_unescaped '$' s
        end;
    | c -> loop start next
  in
  (Buffer.clear b; loop 0 0; Buffer.contents b)

let add_markup_esc k b s start next target_need_escape target_escape =
  let max_i = String.length s - 1 in
  if next > max_i then err_unescaped '\\' s else
  match s.[next] with
  | c when not (is_markup_esc s.[next]) -> err_illegal_esc c s
  | c ->
      (if target_need_escape c then target_escape b c else Buffer.add_char b c);
      k (next + 1) (next + 1)

let add_markup_text k b s start target_need_escape target_escape =
  let max_i = String.length s - 1 in
  let flush start stop = match start > max_i with
  | true -> ()
  | false -> Buffer.add_substring b s start (stop - start + 1)
  in
  let rec loop start i =
    if i > max_i then err_unclosed s else
    let next = i + 1 in
    match s.[i] with
    | '\\' -> (* unescape *)
        flush start (i - 1);
        add_markup_esc loop b s start next target_need_escape target_escape
    | ')' -> flush start (i - 1); k next next
    | c when markup_text_need_esc c -> err_unescaped c s
    | c when target_need_escape c ->
        flush start (i - 1); target_escape b c; loop next next
    | c -> loop start next
  in
  loop start start

(* Plain text output *)

let markup_to_plain b s =
  let max_i = String.length s - 1 in
  let flush start stop = match start > max_i with
  | true -> ()
  | false -> Buffer.add_substring b s start (stop - start + 1)
  in
  let need_escape _ = false in
  let escape _ _ = assert false in
  let rec loop start i =
    if i > max_i then flush start max_i else
    let next = i + 1 in
    match s.[i] with
    | '\\' ->
        flush start (i - 1);
        add_markup_esc loop b s start next need_escape escape
    | '$' ->
        if next > max_i then err_unescaped '$' s else
        begin match s.[next] with
        | '(' ->
            let min = next + 2 in
            if min > max_i then err_unclosed s else
            begin match s.[min] with
            | ',' ->
                let markup = s.[min - 1] in
                if not (is_markup_dir markup) then err_markup markup s else
                let start_data = min + 1 in
                (flush start (i - 1);
                 add_markup_text loop b s start_data need_escape escape)
            | _ -> err_malformed s
            end
        | _ -> err_unescaped '$' s
        end
    | c when markup_need_esc c -> err_unescaped c s
    | c -> loop start next
  in
  (Buffer.clear b; loop 0 0; Buffer.contents b)

let doc_to_plain b ~subst s = markup_to_plain b (subst_vars b ~subst s)

let p_indent = 7                                  (* paragraph indentation. *)
let l_indent = 4                                      (* label indentation. *)

let pp_plain_blocks subst ppf ts =
  let b = Buffer.create 1024 in
  let markup t = doc_to_plain b ~subst t in
  let pp_tokens ppf t = pp_tokens ~spaces:true ppf t in
  let rec loop = function
  | [] -> ()
  | t :: ts ->
      begin match t with
      | `Noblank -> ()
      | `Blocks bs -> loop bs (* not T.R. *)
      | `P s -> pf ppf "%a@[%a@]@," pp_indent p_indent pp_tokens (markup s)
      | `S s -> pf ppf "@[%a@]" pp_tokens (markup s)
      | `Pre s -> pf ppf "%a@[%a@]@," pp_indent p_indent pp_lines (markup s)
      | `I (label, s) ->
          let label = markup label in
          let s = markup s in
          pf ppf "@[%a@[%a@]" pp_indent p_indent pp_tokens label;
          if s = "" then pf ppf "@]@," else
          let ll = String.length label in
          match ll < l_indent with
          | true ->
              pf ppf "%a@[%a@]@]@," pp_indent (l_indent - ll) pp_tokens s
          | false ->
              pf ppf "@\n%a@[%a@]@]@,"
                pp_indent (p_indent + l_indent) pp_tokens s
      end;
      begin match ts with
      | `Noblank :: ts -> loop ts
      | ts -> Format.pp_print_cut ppf (); loop ts
      end
  in
  loop ts

let pp_plain_page subst ppf (_, text) =
  pf ppf "@[<v>%a@]" (pp_plain_blocks subst) text

(* Groff output *)

let markup_to_groff b s =
  let max_i = String.length s - 1 in
  let flush start stop = match start > max_i with
  | true -> ()
  | false -> Buffer.add_substring b s start (stop - start + 1)
  in
  let need_escape = function '.' | '\'' | '-' | '\\' -> true | _ -> false in
  let escape b c = Printf.bprintf b "\\N'%d'" (Char.code c) in
  let rec end_text start i = Buffer.add_string b "\\fR"; loop start i
  and loop start i =
    if i > max_i then flush start max_i else
    let next = i + 1 in
    match s.[i] with
    | '\\' ->
        flush start (i - 1);
        add_markup_esc loop b s start next need_escape escape
    | '$' ->
        if next > max_i then err_unescaped '$' s else
        begin match s.[next] with
        | '(' ->
            let min = next + 2 in
            if min > max_i then err_unclosed s else
            begin match s.[min] with
            | ','  ->
                let start_data = min + 1 in
                flush start (i - 1);
                begin match s.[min - 1] with
                | 'i' -> Buffer.add_string b "\\fI"
                | 'b' -> Buffer.add_string b "\\fB"
                | markup -> err_markup markup s
                end;
                add_markup_text end_text b s start_data need_escape escape
            | _ -> err_malformed s
            end
        | _ -> err_unescaped '$' s
        end
    | c when markup_need_esc c -> err_unescaped c s
    | c when need_escape c ->
        flush start (i - 1); escape b c; loop next next
    | c -> loop start next
  in
  (Buffer.clear b; loop 0 0; Buffer.contents b)

let doc_to_groff b ~subst s = markup_to_groff b (subst_vars b subst s)

let pp_groff_blocks subst ppf text =
  let buf = Buffer.create 1024 in
  let markup t = doc_to_groff ~subst buf t in
  let pp_tokens ppf t = pp_tokens ~spaces:false ppf t in
  let rec pp_block = function
  | `Blocks bs -> List.iter pp_block bs (* not T.R. *)
  | `P s -> pf ppf "@\n.P@\n%a" pp_tokens (markup s)
  | `Pre s -> pf ppf "@\n.P@\n.nf@\n%a@\n.fi" pp_lines (markup s)
  | `S s -> pf ppf "@\n.SH %a" pp_tokens (markup s)
  | `Noblank -> pf ppf "@\n.sp -1"
  | `I (l, s) ->
      pf ppf "@\n.TP 4@\n%a@\n%a" pp_tokens (markup l) pp_tokens (markup s)
  in
  List.iter pp_block text

let pp_groff_page subst ppf ((n, s, a1, a2, a3), t) =
  pf ppf ".\\\" Pipe this output to groff -man -Tutf8 | less@\n\
          .\\\"@\n\
          .TH \"%s\" %d \"%s\" \"%s\" \"%s\"@\n\
          .\\\" Disable hyphenation and ragged-right@\n\
          .nh@\n\
          .ad l\
          %a@?"
    n s a1 a2 a3 (pp_groff_blocks subst) t

(* Printing to a pager *)

let pp_to_temp_file pp_v v =
  try
    let exec = Filename.basename Sys.argv.(0) in
    let file, oc = Filename.open_temp_file exec "out" in
    let ppf = Format.formatter_of_out_channel oc in
    pp_v ppf v; Format.pp_print_flush ppf (); close_out oc;
    at_exit (fun () -> try Sys.remove file with Sys_error e -> ());
    Some file
  with Sys_error _ -> None

let find_cmd cmds =
  let test, null = match Sys.os_type with
  | "Win32" -> "where", " NUL"
  | _ -> "type", "/dev/null"
  in
  let cmd c = Sys.command (strf "%s %s 1>%s 2>%s" test c null null) = 0 in
  try Some (List.find cmd cmds) with Not_found -> None

let pp_to_pager print ppf v =
  let pager =
    let cmds = ["less"; "more"] in
    let cmds = try (Sys.getenv "PAGER") :: cmds with Not_found -> cmds in
    let cmds = try (Sys.getenv "MANPAGER") :: cmds with Not_found -> cmds in
    find_cmd cmds
  in
  match pager with
  | None -> print `Plain ppf v
  | Some pager ->
      let cmd = match (find_cmd ["groff"; "nroff"]) with
      | None ->
          begin match pp_to_temp_file (print `Plain) v with
          | None -> None
          | Some f -> Some (strf "%s < %s" pager f)
          end
      | Some c ->
          begin match pp_to_temp_file (print `Groff) v with
          | None -> None
          | Some f ->
              (* TODO use -Tutf8, but annoyingly maps U+002D to U+2212. *)
              let xroff = if c = "groff" then c ^ " -Tascii -P-c" else c in
              Some (strf "%s -man < %s | %s" xroff f pager)
          end
      in
      match cmd with
      | None -> print `Plain ppf v
      | Some cmd -> if (Sys.command cmd) <> 0 then print `Plain ppf v

(* Output *)

type format = [ `Auto | `Pager | `Plain | `Groff ]

let rec print ?(subst = fun x -> None) fmt ppf page = match fmt with
| `Pager -> pp_to_pager (print ~subst) ppf page
| `Plain -> pp_plain_page subst ppf page
| `Groff -> pp_groff_page subst ppf page
| `Auto ->
    match try (Some (Sys.getenv "TERM")) with Not_found -> None with
    | None | Some "dumb" -> print ~subst `Plain ppf page
    | Some _ -> print ~subst `Pager ppf page

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
