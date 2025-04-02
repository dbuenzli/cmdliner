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

let test_restart_restricted_tool =
  Test.test "Restart restricted tool" @@ fun () ->
  let cmd =
    Cmd.make (Cmd.info "test_restart_restricted") @@
    let+ verb = Arg.(value & flag & info ["verbose"])
    and+ tool =
      let tool = Arg.enum ~docv:"VCS" ["git", `Git; "hg", `Hg] in
      Arg.(required & pos 0 (some tool) None & info [])
    and+ args =
      let arg =
        let completion = Arg.Completion.make ~restart:true () in
        Arg.Conv.of_conv ~docv:"ARG" Arg.string ~completion ()
      in
      Arg.(value & pos_right 0 arg [] & info [])
    in
    ()
  in
  let complete = Testing_cmdliner.snap_completion cmd in
  complete ["+cmdliner_complete:"] @@ __POS_OF__
    "1\n\
     group\n\
     Values\n\
     item\n\
     git\n\
     \n\
     item\n\
     hg\n\
     \n\
     group\n\
     Options\n\
     item\n\
     --verbose\n\
     \n";
  complete ["+cmdliner_complete:g"] @@ __POS_OF__
    "1\n\
     group\n\
     Values\n\
     item\n\
     git\n\
     \n\
     group\n\
     Options\n";
  (* Note no reset here: as there is no -- token *)
  complete ["git"; "+cmdliner_complete:"] @@ __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     --verbose\n\
     \n";
  complete ["--"; "git"; "+cmdliner_complete:"] @@ __POS_OF__
    "1\n\
     restart\n";
  ()

let test_restart_any_tool =
  Test.test "Restart any tool" @@ fun () ->
  let cmd =
    Cmd.make (Cmd.info "test_restart") @@
    let arg ~docv =
      let completion = Arg.Completion.make ~restart:true () in
      Arg.Conv.of_conv ~docv:"TOOL" Arg.string ~completion ()
    in
    let+ verb = Arg.(value & flag & info ["verbose"])
    and+ tool = Arg.(required & pos 0 (some (arg ~docv:"TOOL")) None & info [])
    and+ args = Arg.(value & pos_right 0 (arg ~docv:"ARG") [] & info []) in
    ()
  in
  let complete = Testing_cmdliner.snap_completion cmd in
  complete ["+cmdliner_complete:"] @@ __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     --verbose\n\
     \n";
  (* The following two do not restart because -- is missing *)
  complete ["+cmdliner_complete:gi"] @@ __POS_OF__
    "1\n\
     group\n\
     Options\n";
  complete ["git"; "+cmdliner_complete:"] @@ __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     --verbose\n\
     \n";
  (* These must restart *)
  complete ["--"; "+cmdliner_complete:gi"] @@ __POS_OF__
    "1\n\
     restart\n";
  complete ["--"; "git"; "+cmdliner_complete:"] @@ __POS_OF__
    "1\n\
     restart\n";
  ()


let main () =
  let doc = "Test completion" in
  Test.main ~doc @@ fun () -> Test.autorun ()

let () = if !Sys.interactive then () else exit (main ())
