(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

let tool () = Cmdliner.Cmd.Exit.ok

open Cmdliner
open Cmdliner.Term.Syntax

let tool_cmd =
  let doc = "The tool synopsis is TODO" in
  Cmd.make (Cmd.info "TODO-toolname" ~doc) @@
  let+ unit = Term.const () in
  tool unit

let main () = Cmd.eval' tool_cmd
let () = if !Sys.interactive then () else exit (main ())
