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
    {|group
Subcommands
item
birds
Operate on birds.
item
mammals
Operate on mammals.
item
fishs
Operate on fishs.
item
camels|};
  ()


let main () =
  let doc = "Test completion" in
  Test.main ~doc @@ fun () -> Test.autorun ()

let () = if !Sys.interactive then () else exit (main ())
