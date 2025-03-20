open Cmdliner

type shell = Bash | Zsh

let shell_enum = ["bash", Bash; "zsh", Zsh]

let completion_script shell prog =
  print_endline (
    match shell with 
    | Bash -> Cmdliner_completion.bash_completion prog
    | Zsh -> Cmdliner_completion.zsh_completion prog)

let completion_script_cmd =
  let shell =
    let doc = "Shell program to output the completion script for" in
    Arg.(required & opt (some (enum shell_enum)) None & info ["shell"] ~docv:"SHELL" ~doc)
  in
  let prog =
    let doc = "Program to output the completion script for" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PROGRAM" ~doc)
  in
  let name = "completion-script" in
  let doc = "Output the completion script for the shell." in
  let man = [
    `S Manpage.s_description;
    `P "Output the completion script for the shell. Example usage is the following:";
    `Pre (Printf.sprintf "  eval \"\\$(cmdliner %s --shell zsh myprog)\"" name);
  ] in
  let info = Cmd.info name ~doc ~man in
  Cmd.v info Term.(const completion_script $ shell $ prog)

let main_cmd =
  let doc = "a helper for cmdliner based programs" in
  let info = Cmd.info "cmdliner" ~version:"%%VERSION%%" ~doc in
  let default = Term.(ret (const (fun () -> `Help (`Pager, None)) $ const ())) in
  Cmd.group info ~default [completion_script_cmd]

let () = exit (Cmd.eval main_cmd)
