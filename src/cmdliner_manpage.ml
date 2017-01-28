(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

let err_doc_string s =
  strf "Variable substitution failed on documentation fragment `%s'" s

(* Manpages *)

type format = [ `Auto | `Pager | `Plain | `Groff ]
type title = string * int * string * string * string
type block =
  [ `S of string | `P of string | `Pre of string | `I of string * string
  | `Noblank ]

type t = title * block list

(* Standard sections *)

let s_name = "NAME"
let s_synopsis = "SYNOPSIS"
let s_description = "DESCRIPTION"
let s_commands = "COMMANDS"
let s_arguments = "ARGUMENTS"
let s_options = "OPTIONS"
let s_environment = "ENVIRONMENT"
let s_files = "FILES"
let s_exit_status = "EXIT STATUS"
let s_examples = "EXAMPLES"
let s_bugs = "BUGS"
let s_author = "AUTHOR"
let s_see_also = "SEE ALSO"

(* Formatting tools *)

let pf = Format.fprintf
let pp_str = Format.pp_print_string
let pp_char = Format.pp_print_char
let pp_indent ppf c = for i = 1 to c do pp_char ppf ' ' done

let pp_white_str spaces ppf s =  (* spaces and new lines with Format's funs *)
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

let pp_text = pp_white_str true
let pp_lines = pp_white_str false

let pp_tokens ?(groff = false) ppf s =
  let is_space = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false in
  let len = String.length s in
  let i = ref 0 in
  try
    while (true) do
      while (!i < len && is_space s.[!i]) do incr i done;
      let start = !i in
      if start = len then raise Exit;
      while (!i < len && not (is_space s.[!i]) && not (s.[!i] = '-')) do
        incr i
        done;
      pp_str ppf (String.sub s start (!i - start));
      if !i = len then raise Exit;
      if s.[!i] = '-' then
        (incr i; if groff then pp_str ppf "\\-" else pp_char ppf '-');
      if (!i < len && is_space s.[!i]) then
        (if groff then pp_char ppf ' ' else Format.pp_print_space ppf ())
    done
  with Exit -> ()

let plain_esc c s = match c with 'g' -> "" (* groff specific *) | _ ->  s

let escape subst esc buf s =
  let subst s =
    let len = String.length s in
    if not (len > 1 && s.[1] = ',') then (subst s) else
    if len = 2 then "" else
    esc s.[0] (String.sub s 2 (len - 2))
  in
  try
    Buffer.clear buf; Buffer.add_substitute buf subst s;
    let s = Buffer.contents buf in (* twice for $(i,$(mname)). *)
    Buffer.clear buf; Buffer.add_substitute buf subst s;
    Buffer.contents buf
  with Not_found -> invalid_arg (err_doc_string s)


let pp_to_temp_file pp_v v =
  try
    let exec = Filename.basename Sys.argv.(0) in
    let file, oc = Filename.open_temp_file exec "out" in
    let ppf = Format.formatter_of_out_channel oc in
    pp_v ppf v; Format.pp_print_flush ppf (); close_out oc;
    at_exit (fun () -> try Sys.remove file with Sys_error e -> ());
    Some file
  with Sys_error _ -> None

(* Plain text output *)

let p_indent = 7                                  (* paragraph indentation. *)
let l_indent = 4                                      (* label indentation. *)

let pp_plain_blocks subst ppf ts =
  let buf = Buffer.create 1024 in
  let escape t = escape subst plain_esc buf t in
  let pp_tokens ppf t = pp_tokens ppf (escape t) in
  let rec aux = function
  | [] -> ()
  | t :: ts ->
      begin match t with
      | `Noblank -> ()
      | `P s -> pf ppf "%a@[%a@]@," pp_indent p_indent pp_tokens s
      | `S s -> pf ppf "@[%a@]" pp_tokens s
      | `Pre s -> pf ppf "%a@[%a@]@," pp_indent p_indent pp_lines (escape s)
      | `I (label, s) ->
          let label = escape label in
          let ll = String.length label in
          pf ppf "@[%a@[%a@]" pp_indent p_indent pp_tokens label;
          if s = "" then pf ppf "@]@," else
          if ll < l_indent
          then pf ppf "%a@[%a@]@]@," pp_indent (l_indent - ll) pp_tokens s
          else
          pf ppf "@\n%a@[%a@]@]@," pp_indent (p_indent + l_indent) pp_tokens s
      end;
      begin match ts with
      | `Noblank :: ts -> aux ts
      | ts -> Format.pp_print_cut ppf (); aux ts
      end
  in
  aux ts

let pp_plain_page subst ppf (_, text) =
  pf ppf "@[<v>%a@]" (pp_plain_blocks subst) text

(* Groff output *)

let groff_esc c s = match c with
| 'i' -> (strf "\\fI%s\\fR" s)
| 'b' -> (strf "\\fB%s\\fR" s)
| 'p' -> "" (* plain text specific *)
| _ -> s

let pp_groff_lines ppf s =
  let left = ref 0 and right = ref 0 and len = String.length s in
  let flush () =
    Format.pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    if s.[!right] = '\n' then (flush (); Format.pp_force_newline ppf ()) else
    if s.[!right] = '-' then (flush (); pp_str ppf "\\-") else
    incr right;
  done;
  if !left <> len then flush ()

let pp_groff_blocks subst ppf text =
  let buf = Buffer.create 1024 in
  let escape t = escape subst groff_esc buf t in
  let pp_tokens ppf t = pp_tokens ~groff:true ppf (escape t) in
  let pp_block = function
  | `P s -> pf ppf "@\n.P@\n%a" pp_tokens s
  | `Pre s -> pf ppf "@\n.P@\n.nf@\n%a@\n.fi" pp_groff_lines (escape s)
  | `S s -> pf ppf "@\n.SH %a" pp_tokens s
  | `Noblank -> pf ppf "@\n.sp -1"
  | `I (l, s) -> pf ppf "@\n.TP 4@\n%a@\n%a" pp_tokens l pp_tokens s
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

let rec print ?(subst = fun x -> x) fmt ppf page = match fmt with
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
