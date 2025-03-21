(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

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
_cmdliner_generic
|} toolname
end

let shells : shell list = [(module Bash); (module Zsh)]

let log_write p = Printf.printf "Writing \x1B[1m%s\x1B[0m\n%!" p

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
    log_write path;
    if not dry_run then match write_file path (Shell.generic_completion) with
    | Error e -> failwith e | Ok () -> ()
  in
  List.iter (install ~dry_run sharedir) shells;
  Cmdliner.Cmd.Exit.ok

let install_tool_completion ~dry_run shells ~toolname sharedir =
  with_binary_stdout @@ fun () ->
  let install ~dry_run ~toolname sharedir (module Shell : SHELL) =
    let dest = Filename.concat sharedir Shell.sharedir in
    let path = Filename.concat dest (Shell.tool_script_name ~toolname) in
    log_write path;
    let script = Shell.tool_completion ~toolname in
    if not dry_run then match write_file path script with
    | Error e -> failwith e | Ok () -> ()
  in
  List.iter (install ~dry_run ~toolname sharedir) shells;
  Cmdliner.Cmd.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let dry_run =
  let doc = "Do not install, output paths that would be written." in
  Arg.(value & flag & info ["dry-run"] ~doc)

let sharedir_posn n =
  let doc = "$(docv) is the $(b,share) directory to install to." in
  Arg.(required & pos n (some dir) None & info [] ~doc ~docv:"SHAREDIR")

let sharedir_pos0 = sharedir_posn 0
let sharedir_pos1 = sharedir_posn 1

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
  Arg.(required & pos n (some string) None & info [] ~doc ~docv:"TOOLNAME")

let toolname_pos0 = toolname_posn 0
let toolname_pos1 = toolname_posn 1

let generic_completion_cmd =
  let doc = "Output generic completion scripts" in
  let man =
    [ `S Manpage.s_description;
      `P "$(iname) outputs the generic completion script of a given shell. \
          For example:";
      `Pre "$(iname) $(b,zsh)"; `Noblank;
      `Pre "$(b,eval) \\$($(iname) $(b,zsh))"; ]
  in
  Cmd.make (Cmd.info "generic-completion" ~doc ~man) @@
  let+ shell = shell_pos0 in
  generic_completion shell

let tool_completion_cmd =
  let doc = "Output tool completion scripts" in
  let man =
    [ `S Manpage.s_description;
      `P "$(iname) outputs the tool completion script of a given shell. \
          For example:";
      `Pre "$(iname) $(b,zsh mytool)"; ]
  in
  Cmd.make (Cmd.info "tool-completion" ~doc ~man) @@
  let+ shell = shell_pos0 and+ toolname = toolname_pos1 in
  tool_completion shell ~toolname

let install_generic_completion_cmd =
  let doc = "Install generic completion scripts" in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) installs the generic completion script of given shells in \
        a $(b,share) directory according to specific shell conventions. \
        Directories are not created. \
        Use option $(b,--dry-run) to see which paths would be written. \
        For example:";
    `Pre "$(iname) $(b,/usr/local/share) # All supported shells"; `Noblank;
    `Pre "$(iname) $(b,--shell zsh /usr/local/share)"; ]
  in
  Cmd.make (Cmd.info "generic-completion" ~doc ~man) @@
  let+ dry_run and+ shells = shells_opt and+ sharedir_pos0 in
  install_generic_completion ~dry_run shells sharedir_pos0

let install_tool_completion_cmd =
  let doc = "Install tool completion scripts" in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) installs the tool completion script of given shells in \
        a $(b,share) directory according to specific shell conventions. \
        Directories are not created. \
        Use option $(b,--dry-run) to see which paths would be written. \
        For example:";
    `Pre "$(iname) $(b,mytool) $(b,/usr/local/share)  # All supported shells";
    `Noblank;
    `Pre "$(iname) $(b,--shell zsh mytool /usr/local/share)"; ]
  in
  Cmd.make (Cmd.info "tool-completion" ~doc ~man) @@
  let+ dry_run and+ shells = shells_opt and+ toolname = toolname_pos0
  and+ sharedir_pos1 in
  install_tool_completion ~dry_run shells ~toolname sharedir_pos1

let install_cmd =
  let doc = "Install cmdliner support files" in
  let man =
    [ `S Manpage.s_description;
      `P "$(iname) subcommands install cmdliner support files. \
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
      `P "$(mname) is a helper tool for Cmdliner based programs. It \
          helps with installing command line completion scripts. \
          See the library documentation or invoke \
          subcommands with $(b,--help) for more details."; ]
  in
  Cmd.group (Cmd.info "cmdliner" ~version:"%%VERSION%%" ~doc ~man) ~default @@
  [generic_completion_cmd; tool_completion_cmd; install_cmd]

let main () = Cmd.eval' main_cmd
let () = if !Sys.interactive then () else exit (main ())
