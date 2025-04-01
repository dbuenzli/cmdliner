(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

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
  let write file s oc = try Ok (Out_channel.output_string oc s) with
  | Sys_error e -> Error (Printf.sprintf "%s: %s" file e)
  in
  let binary_stdout () = Out_channel.(set_binary_mode stdout true) in
  try match file with
  | "-" -> binary_stdout (); write file s Out_channel.stdout
  | file -> Out_channel.with_open_bin file (write file s)
  with Sys_error e -> Error e

let with_binary_stdout f =
  try let () = Out_channel.set_binary_mode stdout true in f () with
  | Sys_error e | Failure e -> prerr_endline e; Cmdliner.Cmd.Exit.some_error

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

(* Shells *)

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
  _comp_load _cmdliner_generic
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

let install_tool_completion ~dry_run shells ~toolnames sharedir =
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

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let dry_run =
  let doc = "Do not install, output paths that would be written." in
  Arg.(value & flag & info ["dry-run"] ~doc)

let sharedir_posn ~rev n =
  let doc = "$(docv) is the $(b,share) directory to install to." in
  Arg.(required & pos ~rev n (some dirpath) None &
       info [] ~doc ~docv:"SHAREDIR")

let sharedir_pos0 = sharedir_posn ~rev:false 0
let sharedir_poslast = sharedir_posn ~rev:true 0

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
  let doc = "$(docv) is the name of the tool to complete" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"TOOLNAME")

let toolname_pos0 = toolname_posn 0
let toolname_pos1 = toolname_posn 1
let toolnames_posleft =
  let doc = "$(docv) is the name of the tool to complete. Repeatable." in
  Arg.(non_empty & pos_left ~rev:true 0 string [] &
       info [] ~doc ~docv:"TOOLNAME")

let generic_completion_cmd =
  let doc = "Output generic completion scripts" in
  let man =
    [ `S Manpage.s_description;
      `P "$(cmd) outputs the generic completion script of a given shell. \
          For example:";
      `Pre "$(cmd) $(b,zsh)"; `Noblank;
      `Pre "$(b,eval) \\$($(cmd) $(b,zsh))"; ]
  in
  Cmd.make (Cmd.info "generic-completion" ~doc ~man) @@
  let+ shell = shell_pos0 in
  generic_completion shell

let tool_completion_cmd =
  let doc = "Output tool completion scripts" in
  let man =
    [ `S Manpage.s_description;
      `P "$(cmd) outputs the tool completion script of a given shell. \
          For example:";
      `Pre "$(cmd) $(b,zsh mytool)"; ]
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
        Directories are not created. \
        Use option $(b,--dry-run) to see which paths would be written. \
        For example:";
    `Pre "$(cmd) $(b,/usr/local/share) # All supported shells"; `Noblank;
    `Pre "$(cmd) $(b,--shell zsh /usr/local/share)"; ]
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
    `P "$(cmd) installs the tool completion script of given shells in \
        a $(b,share) directory according to specific shell conventions. \
        Directories are not created. \
        Use option $(b,--dry-run) to see which paths would be written. \
        For example:";
    `Pre "$(cmd) $(b,mytool) $(b,/usr/local/share)  # All supported shells";
    `Noblank;
    `Pre "$(cmd) $(b,--shell zsh mytool /usr/local/share)"; ]
  in
  Cmd.make (Cmd.info "tool-completion" ~doc ~man) @@
  (* No let punning in < 4.13 *)
  let+ dry_run = dry_run and+ shells = shells_opt
  and+ toolnames = toolnames_posleft and+ sharedir_poslast = sharedir_poslast in
  install_tool_completion ~dry_run shells ~toolnames sharedir_poslast

let install_cmd =
  let doc = "Install cmdliner support files" in
  let man =
    [ `S Manpage.s_description;
      `P "$(cmd) subcommands install cmdliner support files. \
          See the library documentation or invoke \
          subcommands with $(b,--help) for more details."; ]
  in
  Cmd.group (Cmd.info "install" ~doc ~man) @@
  [install_generic_completion_cmd; install_tool_completion_cmd]

let main_cmd =
  let doc = "Helper tool for cmdliner based programs" in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  let man =
    [ `S Manpage.s_description;
      `P "$(tool) is a helper tool for Cmdliner based programs. It \
          helps with installing command line completion scripts. \
          See the library documentation or invoke \
          subcommands with $(b,--help) for more details."; ]
  in
  Cmd.group (Cmd.info "cmdliner" ~version:"%%VERSION%%" ~doc ~man) ~default @@
  [generic_completion_cmd; tool_completion_cmd; install_cmd]

let main () = Cmd.eval' main_cmd
let () = if !Sys.interactive then () else exit (main ())
