(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Use to test for completion interactively. *)

let tool ~file ~dir ~path ~choice ~cmd =
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
  and+ choice =
    let doc =
      "Use me to test for enum completion for short and long options."
    in
    Arg.(value & opt (enum ["one", `One; "two", `Two; "three", `Three]) `One
         & info ["choice"; "c"] ~doc)
  and+ cmd =
    let arg_conv =
      let completion = Arg.Completion.complete_restart in
      Arg.Conv.of_conv ~completion Arg.string
    in
    let doc =
      "Use me to test for completion restart of other tools after \
       the $(b,--) token. This may not work through a $(b,b0) \
       invocation. Install completion for the $(b,test_shell) tool \
       and invoke the tool directly from the path output with
       $(b,b0 --path -- test_shell)."
    in
    Arg.(value & pos_all arg_conv [] & info [] ~doc ~docv:"ARG")
  in
  tool ~file ~dir ~path ~choice ~cmd

let main () = Cmd.eval' cmd
let () = if !Sys.interactive then () else exit (main ())
