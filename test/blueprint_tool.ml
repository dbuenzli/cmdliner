(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

let tool ~flag ~infile = Cmdliner.Cmd.Exit.ok

open Cmdliner
open Cmdliner.Term.Syntax

let flag = Arg.(value & flag & info ["flag"] ~doc:"The flag")
let infile =
  let doc = "$(docv) is the input file. Use $(b,-) for $(b,stdin)." in
  Arg.(value & pos 0 file "-" & info [] ~doc ~docv:"FILE")

let tool =
  let doc = "The tool synopsis is TODO" in
  Cmd.make (Cmd.info "TODO-toolname" ~version:"%%VERSION%%" ~doc) @@
  let+ flag and+ infile in
  tool ~flag ~infile

let main () = Cmd.eval' tool
let () = if !Sys.interactive then () else exit (main ())
