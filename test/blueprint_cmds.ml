(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

let hey () = Cmdliner.Cmd.Exit.ok
let ho () = Cmdliner.Cmd.Exit.ok

open Cmdliner
open Cmdliner.Term.Syntax

let flag = Arg.(value & flag & info ["flag"] ~doc:"The flag")
let infile =
  let doc = "$(docv) is the input file. Use $(b,-) for $(b,stdin)." in
  Arg.(value & pos 0 file "-" & info [] ~doc ~docv:"FILE")

let hey_cmd =
  let doc = "The hey command synopsis is TODO" in
  Cmd.make (Cmd.info "hey" ~doc) @@
  let+ unit = Term.const () in
  ho ()

let ho_cmd =
  let doc = "The ho command synopsis is TODO" in
  Cmd.make (Cmd.info "ho" ~doc) @@
  let+ unit = Term.const () in
  ho unit

let tool =
  let doc = "The tool synopsis is TODO" in
  Cmd.group (Cmd.info "TODO-toolname" ~version:"%%VERSION%%" ~doc) @@
  [hey_cmd; ho_cmd]

let main () = Cmd.eval' tool
let () = if !Sys.interactive then () else exit (main ())
