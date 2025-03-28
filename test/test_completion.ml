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

let test_groups =
  Test.test "Cmd.group completions" @@ fun () ->
  let cmd = Testing_cmdliner.sample_group_cmd in
  let complete = Testing_cmdliner.snap_completion cmd in
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
camels";
  complete ["birds"; "+cmdliner_complete:"] @@  __POS_OF__
"";

  ()


let main () =
  let doc = "Test completion" in
  Test.main ~doc @@ fun () -> Test.autorun ()

let () = if !Sys.interactive then () else exit (main ())
