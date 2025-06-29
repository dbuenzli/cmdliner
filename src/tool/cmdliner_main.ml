(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf
let error_to_failure = function Ok v -> v | Error e -> failwith e

let rec mkdir dir = (* Can be replaced by Sys.mkdir once we drop OCaml < 4.12 *)
  (* On Windows -p does not exist we do it ourselves on all platforms. *)
  let err_cmd exit cmd =
    raise (Sys_error (strf "exited with %d: %s\n" exit cmd))
  in
  let quote_cmd = if Sys.win32 then fun cmd -> strf "\"%s\"" cmd else Fun.id in
  let run_cmd args =
    let cmd = String.concat " " (List.map Filename.quote args) in
    let exit = Sys.command (quote_cmd cmd) in
    if exit = 0 then () else err_cmd exit cmd
  in
  let parent = Filename.dirname dir in
  (if String.equal dir parent then () else mkdir (Filename.dirname dir));
  (if Sys.file_exists dir then () else run_cmd ["mkdir"; dir])

let write_file file s =
  (* Out_channel is < 4.14 *)
  let write file s oc = try Ok (output_string oc s) with
  | Sys_error e -> Error (Printf.sprintf "%s: %s" file e)
  in
  let binary_stdout () = set_binary_mode_out stdout true in
  try match file with
  | "-" -> binary_stdout (); write file s stdout
  | file ->
      let oc = open_out_bin file in
      let finally () = close_out_noerr oc in
      Fun.protect ~finally @@ fun () -> write file s oc
  with Sys_error e -> Error e

let with_binary_stdout f =
  try let () = set_binary_mode_out stdout true in f () with
  | Sys_error e | Failure e -> prerr_endline e; Cmdliner.Cmd.Exit.some_error

let exec_stdout cmd =
  try
    let tmp = Filename.temp_file "cmd" "stdout" in
    let exec = String.concat " > " [cmd; Filename.quote tmp] in
    match Sys.command exec with
    | 0 ->
        let ic = open_in_bin tmp in
        let finally () = close_in_noerr ic in
        let len = in_channel_length ic in
        Fun.protect ~finally @@ fun () ->
        let stdout = really_input_string ic len in
        Sys.remove tmp;
        Ok stdout
    | exit -> Error (strf "%s: exited with %d" exec exit)
  with
  | Sys_error e -> Error e

(* Cmdliner based tool introspection.

   Note this is a bit hackish but does the job. At some point we could
   investigate cleaner protocols with the `--cmdliner` reserved option. *)

let split_toolname toolexec =
  let tool, name = Scanf.sscanf toolexec "%s@:%s" (fun n e -> n, e) in
  let name =
    if name <> "" then name else
    let name = Filename.basename tool in
    match Filename.chop_suffix_opt ~suffix:".exe" tool with
    | None -> name | Some name -> name
  in
  tool, name

let get_tool_commands tool =
  (* We get that by using the completion protocol, see doc/cli.mld  *)
  try
    let subcommands cmd =
      let rec find_subs = function
      | "group" :: "Subcommands" :: lines ->
          let rec subs acc = function
          | "group" :: _ | [] -> acc
          | "item" :: sub :: lines ->
              let sub = if cmd = "" then sub else String.concat " " [cmd; sub]in
              subs (sub :: acc) lines
          | _ :: lines -> subs acc lines
          in
          subs [] lines
      | _ :: lines -> find_subs lines
      | [] -> []
      in
      let exec = strf "%s --__complete %s --__complete=" tool cmd in
      let comps = exec_stdout exec |> error_to_failure in
      let comps = String.split_on_char '\n' comps in
      match comps with
      | "1" :: comps -> find_subs comps
      | version :: comps ->
          failwith (strf "Unsupported cmdliner completion protocol: %S" version)
      | [] ->
          failwith "Could not parse cmdliner completion protocol"
    in
    let rec loop acc = function
    | cmd :: cmds ->
        let subs = subcommands cmd in
        loop (if cmd <> "" then cmd :: acc else acc) (List.rev_append subs cmds)
    | [] -> List.sort String.compare acc
    in
    Ok (loop [] [""])
  with Failure e -> Error e

let get_tool_command_man tool ~name cmd =
  let exec = if cmd = "" then tool else String.concat " " [tool; cmd] in
  let man_basename =
    let exec = if cmd = "" then name else String.concat " " [name; cmd] in
    (String.map (function ' ' -> '-' | c -> c) exec)
  in
  let add_section man =
    let rec extract_section = function
    | line :: lines ->
        begin match Scanf.sscanf line ".TH %s %d"  (fun _ n -> n) with
        | n -> Ok (n, man_basename, man)
        | exception Scanf.Scan_failure _ -> extract_section lines
        end
    | [] ->
        Error (strf "%s command: Could not extract section from manual" exec)
    in
    extract_section (String.split_on_char '\n' man)
  in
  let get_groff = exec ^ " --help=groff" in
  match exec_stdout get_groff with
  | Error _ as e -> e
  | Ok man -> add_section man

let get_tool_manpages tool ~name = match get_tool_commands tool with
| Error _ as e -> e
| Ok cmds ->
    try
      let man cmd = match get_tool_command_man tool ~name cmd with
      | Error e -> failwith e
      | Ok man -> man
      in
      Ok (List.sort compare (List.map man ("" :: cmds)))
    with
    | Failure e -> Error e

(* File path actions *)

let log_action act p = Printf.printf "%s \x1B[1m%s\x1B[0m\n%!" act p

let mkdir ~dry_run p =
  if not (Sys.file_exists p) then begin
    log_action "Creating directory" p;
    if not dry_run then mkdir p
  end

let write_file ~dry_run p contents =
  log_action "Writing" p;
  if not dry_run then begin match write_file p contents with
  | Ok () -> ()
  | Error e -> failwith e
  end

(* Shells completion *)

module type SHELL = sig
  val name : string
  val sharedir : string
  val generic_script_name : string
  val generic_completion : string
  val tool_script_name : toolname:string -> string
  val tool_completion : toolname:string -> string
end

type shell = (module SHELL)

module Bash = struct
  let name = "bash"
  let sharedir = "bash-completion/completions"
  let generic_script_name = "_cmdliner_generic"
  let generic_completion = Cmdliner_data.bash_generic_completion
  let tool_script_name ~toolname = toolname
  let tool_completion ~toolname = strf
{|if ! declare -F _cmdliner_generic > /dev/null; then
  _completion_loader _cmdliner_generic
fi
complete -F _cmdliner_generic %s
|} toolname
end

module Zsh = struct
  let name = "zsh"
  let sharedir = "zsh/site-functions"
  let generic_script_name = "_cmdliner_generic"
  let generic_completion = Cmdliner_data.zsh_generic_completion
  let tool_script_name ~toolname = "_" ^ toolname
  let tool_completion ~toolname = strf
{|#compdef %s
autoload _cmdliner_generic
_cmdliner_generic
|} toolname
end

let shells : shell list = [(module Bash); (module Zsh)]

let generic_completion (module Shell : SHELL) =
  with_binary_stdout @@ fun () ->
  print_string Shell.generic_completion;
  Cmdliner.Cmd.Exit.ok

let tool_completion (module Shell : SHELL) ~toolname =
  with_binary_stdout @@ fun () ->
  print_string (Shell.tool_completion ~toolname);
  Cmdliner.Cmd.Exit.ok

(* Install commands *)

let install_generic_completion ~dry_run shells sharedir =
  with_binary_stdout @@ fun () ->
  let install ~dry_run sharedir (module Shell : SHELL) =
    let dest = Filename.concat sharedir Shell.sharedir in
    let path = Filename.concat dest Shell.generic_script_name in
    mkdir ~dry_run dest;
    write_file ~dry_run path Shell.generic_completion;
  in
  List.iter (install ~dry_run sharedir) shells;
  Cmdliner.Cmd.Exit.ok

let install_tool_completion ~dry_run ~shells ~toolnames ~sharedir =
  with_binary_stdout @@ fun () ->
  let install ~dry_run ~toolnames sharedir (module Shell : SHELL) =
    let dest = Filename.concat sharedir Shell.sharedir in
    let write toolname =
      let path = Filename.concat dest (Shell.tool_script_name ~toolname) in
      write_file ~dry_run path (Shell.tool_completion ~toolname)
    in
    mkdir ~dry_run dest;
    List.iter write toolnames
  in
  List.iter (install ~dry_run ~toolnames sharedir) shells;
  Cmdliner.Cmd.Exit.ok

let install_tool_manpages ~dry_run ~tools ~mandir =
  (* Note this correctly handles manpages sections but at the moment
     all manpages for tool and commands are in section 1. *)
  let rec get_mans tool =
    let tool, name = split_toolname tool in
    match get_tool_manpages tool ~name with
    | Ok mans -> mans
    | Error e -> failwith e
  in
  try
    let mans = List.sort compare (List.concat (List.map get_mans tools)) in
    let rec install ~dry_run ~last_sec = function
    | (sec, basename, man) :: mans ->
        let mandir = Filename.concat mandir (strf "man%d" sec) in
        let manfile = Filename.concat mandir (strf "%s.%d" basename sec) in
        if last_sec <> sec then mkdir ~dry_run mandir;
        write_file ~dry_run manfile man;
        install ~dry_run ~last_sec:sec mans
    | [] -> ()
    in
    install ~dry_run ~last_sec:(-1) mans;
    Cmdliner.Cmd.Exit.ok
  with Failure e -> prerr_endline e; Cmdliner.Cmd.Exit.some_error

let install_tool_support ~dry_run tools shells ~prefix ~sharedir ~mandir =
  let sharedir = match sharedir with
  | None -> Filename.concat prefix "share" | Some sharedir -> sharedir
  in
  let mandir = match mandir with
  | None -> Filename.concat sharedir "man" | Some mandir -> mandir
  in
  let rc = install_tool_manpages ~dry_run ~tools ~mandir in
  if rc <> Cmdliner.Cmd.Exit.ok then rc else
  let toolnames = List.map snd (List.map split_toolname tools) in
  install_tool_completion ~dry_run ~shells ~toolnames ~sharedir

(* Tool command listing command *)

let tool_commands tool = match get_tool_commands tool with
| Ok subs -> List.iter print_endline subs; Cmdliner.Cmd.Exit.ok
| Error e -> prerr_endline e; Cmdliner.Cmd.Exit.some_error

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let dry_run =
  let doc = "Do not install, output paths that would be written." in
  Arg.(value & flag & info ["dry-run"] ~doc)

let prefix =
  let doc = "$(docv) is the install prefix. For example $(b,/usr/local)." in
  Arg.(required & pos ~rev:true 0 (some dirpath) None &
       info [] ~doc ~docv:"PREFIX")

let sharedir_doc = "$(docv) is the $(b,share) directory to install to."
let sharedir_docv = "SHAREDIR"
let sharedir_posn ~rev n =
  Arg.(required & pos ~rev n (some dirpath) None &
       info [] ~doc:sharedir_doc ~docv:sharedir_docv)

let sharedir_pos0 = sharedir_posn ~rev:false 0
let sharedir_poslast = sharedir_posn ~rev:true 0
let sharedir_opt =
  let absent = "$(i,PREFIX)$(b,/share)" in
  Arg.(value & opt (some dirpath) None &
       info ["sharedir"] ~doc:sharedir_doc ~docv:sharedir_docv ~absent)

let mandir_doc = "$(docv) is the root $(b,man) directory to install to."
let mandir_docv = "MANDIR"
let mandir_poslast =
  Arg.(required & pos ~rev:true 0 (some dirpath) None &
       info [] ~doc:mandir_doc ~docv:mandir_docv)

let mandir_opt =
  let absent = "$(i,SHAREDIR)$(b,/man)" in
  Arg.(value & opt (some dirpath) None &
       info ["mandir"] ~doc:mandir_doc ~docv:mandir_docv ~absent)

let shell_assoc = List.map (fun ((module S : SHELL) as s) -> S.name, s) shells
let shells_doc = Arg.doc_alts_enum shell_assoc
let shell_conv = Arg.enum ~docv:"SHELL" shell_assoc
let shell_doc = strf "$(docv) the shell to support, must be %s." shells_doc
let shells_opt =
  let doc = shell_doc ^ " Repeatable." in
  let absent = "All supported shells" in
  Arg.(value & opt_all shell_conv shells & info ["shell"] ~absent ~doc)

let shell_posn n =
  Arg.(required & pos n (some shell_conv) None & info [] ~doc:shell_doc)

let shell_pos0 = shell_posn 0
let shell_pos1 = shell_posn 1

let toolname_posn n =
  let doc = "$(docv) is the name of the tool to complete." in
  Arg.(required & pos n (some filepath) None & info [] ~doc ~docv:"TOOLNAME")

let toolname_pos0 = toolname_posn 0
let toolname_pos1 = toolname_posn 1
let toolnames_posleft =
  let doc = "$(docv) is the name of the tool to complete. Repeatable." in
  Arg.(non_empty & pos_left ~rev:true 0 string [] &
       info [] ~doc ~docv:"TOOLNAME")

let tools_posleft =
  let doc =
    "$(i,TOOLEXEC) is the tool executable. Searched in the $(b,PATH) unless \
     an explicit file path is specified. $(i,NAME) is the tool name, if \
     unspecified derived from $(i,TOOLEXEC) by taking the basename and \
     stripping any $(b,.exe) extension. Repeatable."
  in
  let docv = "TOOLEXEC[:NAME]" in
  Arg.(non_empty & pos_left ~rev:true 0 filepath [] & info [] ~doc ~docv)

let generic_completion_cmd =
  let doc = "Output generic completion scripts" in
  let man =
    [ `S Manpage.s_description;
      `P "$(cmd) outputs the generic cmdliner completion script for a given \
          shell. Examples:";
      `Pre "$(cmd) $(b,zsh)"; `Noblank;
      `Pre "$(b,eval) $(b,\\$\\()$(cmd) $(b,zsh\\))";
      `P "The script needs to be loaded in a shell for tool specific \
          scripts output by the command $(b,tool-completion) to work. See \
          command $(b,install generic-completion) to install them.";
    ]
  in
  Cmd.make (Cmd.info "generic-completion" ~doc ~man) @@
  let+ shell = shell_pos0 in
  generic_completion shell

let tool_commands_cmd =
  let doc = "Output all subcommands of a cmdliner tool" in
  let man =
    [ `S Manpage.s_description;
      `P "$(cmd) outputs all the subcommands of a given cmdliner based \
          tool, one per line. Examples:";
      `Pre "$(cmd) $(b,./mytool)"; `Noblank;
      `Pre "$(cmd) $(b,cmdliner)";
    ]
  in
  Cmd.make (Cmd.info "tool-commands" ~doc ~man) @@
  let+ tool =
    let doc =
      "$(docv) is the tool executable. Searched in the $(b,PATH) unless \
       an explicit file path is specified."
    in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"TOOLEXEC")
  in
  tool_commands tool

let tool_completion_cmd =
  let doc = "Output tool completion scripts" in
  let man =
    [ `S Manpage.s_description;
      `P "$(cmd) outputs the tool specific completion script of a given shell. \
          Example:";
      `Pre "$(cmd) $(b,zsh mytool)";
      `P "Note that tool specific completion script need the corresponding \
          generic completion script output by $(b,generic-completion) to be \
          loaded in the shell. To install these scripts see command \
          $(b,install tool-completion).";
    ]
  in
  Cmd.make (Cmd.info "tool-completion" ~doc ~man) @@
  let+ shell = shell_pos0 and+ toolname = toolname_pos1 in
  tool_completion shell ~toolname

let install_generic_completion_cmd =
  let doc = "Install generic completion scripts" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) installs the generic completion script of given shells in \
        a $(b,share) directory according to specific shell conventions. \
        Directories are created if needed. \
        Use option $(b,--dry-run) to see which paths would be written. \
        Examples:";
    `Pre "$(cmd) $(b,/usr/local/share) # All supported shells"; `Noblank;
    `Pre "$(cmd) $(b,--shell zsh /usr/local/share)";
    `P "To inspect the actual scripts use the command \
        $(b,generic-completion).";
  ]
  in
  Cmd.make (Cmd.info "generic-completion" ~doc ~man) @@
  (* No let punning in < 4.13 *)
  let+ dry_run = dry_run and+ shells = shells_opt
  and+ sharedir_pos0 = sharedir_pos0 in
  install_generic_completion ~dry_run shells sharedir_pos0

let install_tool_completion_cmd =
  let doc = "Install tool completion scripts" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) installs tool completion script of given tools and shells in \
        a $(b,share) directory according to specific shell conventions. \
        Directories are created if needed. \
        Use option $(b,--dry-run) to see which paths would be written. \
        Example:";
    `Pre "$(cmd) $(b,mytool) $(b,/usr/local/share)  # All supported shells";
    `Noblank;
    `Pre "$(cmd) $(b,--shell zsh mytool /usr/local/share)";
    `P "Note that the command $(b,install tool-support) also installs \
        completions like this command does. To inspect the actual scripts \
        use the command $(b,tool-completion).";
  ]
  in
  Cmd.make (Cmd.info "tool-completion" ~doc ~man) @@
  (* No let punning in < 4.13 *)
  let+ dry_run = dry_run and+ shells = shells_opt
  and+ toolnames = toolnames_posleft and+ sharedir = sharedir_poslast in
  install_tool_completion ~dry_run ~shells ~toolnames ~sharedir

let install_tool_manpages_cmd =
  let doc = "Install tool and subcommand manpages" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) installs the manpages of the tool and its commands \
        according in directories of a $(b,man) directory. Directories are \
        created if needed. \
        Use option $(b,--dry-run) to see which paths would be written. \
        Example:";
    `Pre "$(cmd) $(b,./mytool) $(b,/usr/local/share/man)";
    `P "Note that the command $(b,install tool-support) also installs manpages \
        like this command does."
  ]
  in
  Cmd.make (Cmd.info "tool-manpages" ~doc ~man) @@
  let+ dry_run = dry_run
  and+ tools = tools_posleft and+ mandir = mandir_poslast in
  install_tool_manpages ~dry_run ~tools ~mandir

let install_tool_support_cmd =
  let doc = "Install both tool completion and manpages" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) combines commands $(b,install tool-completion) and \
        $(b,install tool-manpages) to install all tool support files \
        in a given $(i,PREFIX) which is assumed to follow the Filesystem \
        Hierarchy Standard.
        Use options $(b,--sharedir) and/or $(b,--mandir) if that is
        not the case (e.g. in $(b,opam) as of writing).
        Use option $(b,--dry-run) to see which paths would be written. \
        Example:";
    `Pre "$(cmd) $(b,./mytool) $(b,/usr/local)"; ]
  in
  Cmd.make (Cmd.info "tool-support" ~doc ~man) @@
  let+ dry_run = dry_run and+ shells = shells_opt
  and+ tools = tools_posleft and+ sharedir = sharedir_opt
  and+ mandir = mandir_opt and+ prefix = prefix in
  install_tool_support ~dry_run tools shells ~prefix ~sharedir ~mandir

let install_cmd =
  let doc = "Install support files for cmdliner tools" in
  let man =
    [ `S Manpage.s_description;
      `P "$(cmd) subcommands install cmdliner support files. \
          See the library documentation or invoke \
          subcommands with $(b,--help) for more details."; ]
  in
  Cmd.group (Cmd.info "install" ~doc ~man) @@
  [install_generic_completion_cmd; install_tool_completion_cmd;
   install_tool_manpages_cmd; install_tool_support_cmd]

let main_cmd =
  let doc = "Helper tool for cmdliner based tools" in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  let man =
    [ `S Manpage.s_description;
      `P "$(tool) is a helper for tools using the cmdliner command line \
          interface library. It helps with installing command line \
          completion scripts and manpages. See the library documentation or \
          invoke subcommands with $(b,--help) for more details."; ]
  in
  Cmd.group (Cmd.info "cmdliner" ~version:"%%VERSION%%" ~doc ~man) ~default @@
  [generic_completion_cmd; tool_commands_cmd; tool_completion_cmd; install_cmd]

let main () = Cmd.eval' main_cmd
let () = if !Sys.interactive then () else exit (main ())
