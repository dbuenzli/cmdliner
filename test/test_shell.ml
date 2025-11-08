(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Use to test for completion interactively. *)

let tool ~file ~dir ~path =
  print_endline "Happy?";
  Cmdliner.Cmd.Exit.ok

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  Cmd.make (Cmd.info "test_shell" ~version:"%%VERSION%%") @@
  let+ file =
    let doc = "Use me to test for filepath completion." in
    Arg.(value & opt (some filepath) None & info ["file"] ~doc)
  and+ dir =
    let doc = "Use me to test for dirpath completion." in
    Arg.(value & opt (some dirpath) None & info ["dir"] ~doc)
  and+ path =
    let doc  = "Use me to test for path completion." in
    Arg.(value & opt (some path) None & info ["path"] ~doc)
  in
  tool ~file ~dir ~path

let main () = Cmd.eval' cmd
let () = if !Sys.interactive then () else exit (main ())
