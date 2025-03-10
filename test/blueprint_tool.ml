(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

let exit_todo = 1

let tool ~flag ~infile = exit_todo

open Cmdliner
open Cmdliner.Term.Syntax

let flag = Arg.(value & flag & info ["flag"] ~doc:"The flag")
let infile =
  let doc = "$(docv) is the input file. Use $(b,-) for $(b,stdin)." in
  Arg.(value & pos 0 file "-" & info [] ~doc ~docv:"FILE")

let tool =
  let doc = "The tool synopsis is TODO" in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) does TODO" ]
  in
  let exits =
    Cmd.Exit.info exit_todo ~doc:"When there is stuff todo" ::
    Cmd.Exit.defaults
  in
  Cmd.make (Cmd.info "TODO" ~version:"%%VERSION%%" ~doc ~man ~exits) @@
  let+ flag and+ infile in
  tool ~flag ~infile

let main () = Cmd.eval' tool
let () = if !Sys.interactive then () else exit (main ())
