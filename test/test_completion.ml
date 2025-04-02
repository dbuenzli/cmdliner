(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Cmdliner
open Cmdliner.Term.Syntax

(* The tests have the following structure:

   let test =
     let cmd = … (* A command definition *) in
     (* A few snapshots of completion protocol results *)
     complete … *)

let cmd = Testing_cmdliner.sample_group_cmd
let complete = Testing_cmdliner.snap_completion cmd

let test_groups =
  Test.test "Cmd.group completions" @@ fun () ->
  complete ["+cmdliner_complete:"] @@  __POS_OF__
    "1\n\
     group\n\
     Subcommands\n\
     item\n\
     birds\n\
     Operate on birds.\n\
     item\n\
     mammals\n\
     Operate on mammals.\n\
     item\n\
     fishs\n\
     Operate on fishs.\n\
     item\n\
     camels\n\
     Operate on camels.\n";
  complete ["birds"; "+cmdliner_complete:"] @@  __POS_OF__
    "1\n\
     group\n\
     Subcommands\n\
     item\n\
     fly\n\
     Fly birds.\n\
     item\n\
     land\n\
     Land birds.\n";
  ()

let test_no_options_after_dashsash =
  Test.test "No options after --" @@ fun () ->
  complete ["birds"; "fly"; "+cmdliner_complete:"] @@ __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     --speed\n\
     Movement \u{001B}[04mSPEED\u{001B}[m in m/s\n";
  complete ["birds"; "fly"; "--"; "+cmdliner_complete:"] @@ __POS_OF__
    "1\n";
  ()


let main () =
  let doc = "Test completion" in
  Test.main ~doc @@ fun () -> Test.autorun ()

let () = if !Sys.interactive then () else exit (main ())
