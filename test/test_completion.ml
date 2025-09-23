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
  complete ["--__complete="] @@  __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     --version\n\
     Show version information.\n\
     item-end\n\
     item\n\
     --help\n\
     Show this help in format \u{001B}[04mFMT\u{001B}[m. The value \u{001B}[04mFMT\u{001B}[m must be one of \u{001B}[01mauto\u{001B}[m, \u{001B}[01mpager\u{001B}[m, \u{001B}[01mgroff\u{001B}[m\n\
     or \u{001B}[01mplain\u{001B}[m. With \u{001B}[01mauto\u{001B}[m, the format is \u{001B}[01mpager\u{001B}[m or \u{001B}[01mplain\u{001B}[m whenever the \u{001B}[01mTERM\u{001B}[m env var\n\
     is \u{001B}[01mdumb\u{001B}[m or undefined.\n\
     item-end\n\
     group\n\
     Subcommands\n\
     item\n\
     birds\n\
     Operate on birds.\n\
     item-end\n\
     item\n\
     mammals\n\
     Operate on mammals.\n\
     item-end\n\
     item\n\
     fishs\n\
     Operate on fishs.\n\
     item-end\n\
     item\n\
     camels\n\
     Operate on camels.\n\
     item-end\n\
     item\n\
     lookup\n\
     Lookup animal by name.\n\
     item-end\n";
  complete ["birds"; "--__complete="] @@  __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     -k\n\
     Kind of entity\n\
     item-end\n\
     item\n\
     --kind\n\
     Kind of entity\n\
     item-end\n\
     item\n\
     --can-fly\n\
     \u{001B}[04mBOOL\u{001B}[m indicates if the entity can fly.\n\
     item-end\n\
     item\n\
     --version\n\
     Show version information.\n\
     item-end\n\
     item\n\
     --help\n\
     Show this help in format \u{001B}[04mFMT\u{001B}[m. The value \u{001B}[04mFMT\u{001B}[m must be one of \u{001B}[01mauto\u{001B}[m, \u{001B}[01mpager\u{001B}[m, \u{001B}[01mgroff\u{001B}[m\n\
     or \u{001B}[01mplain\u{001B}[m. With \u{001B}[01mauto\u{001B}[m, the format is \u{001B}[01mpager\u{001B}[m or \u{001B}[01mplain\u{001B}[m whenever the \u{001B}[01mTERM\u{001B}[m env var\n\
     is \u{001B}[01mdumb\u{001B}[m or undefined.\n\
     item-end\n\
     group\n\
     Subcommands\n\
     item\n\
     fly\n\
     Fly birds.\n\
     item-end\n\
     item\n\
     land\n\
     Land birds.\n\
     item-end\n";
  ()

let test_no_options_after_dashsash =
  Test.test "no options after --" @@ fun () ->
  complete ["birds"; "fly"; "--__complete="] @@ __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     --speed\n\
     Movement \u{001B}[04mSPEED\u{001B}[m in m/s\n\
     item-end\n\
     item\n\
     --version\n\
     Show version information.\n\
     item-end\n\
     item\n\
     --help\n\
     Show this help in format \u{001B}[04mFMT\u{001B}[m. The value \u{001B}[04mFMT\u{001B}[m must be one of \u{001B}[01mauto\u{001B}[m, \u{001B}[01mpager\u{001B}[m, \u{001B}[01mgroff\u{001B}[m\n\
     or \u{001B}[01mplain\u{001B}[m. With \u{001B}[01mauto\u{001B}[m, the format is \u{001B}[01mpager\u{001B}[m or \u{001B}[01mplain\u{001B}[m whenever the \u{001B}[01mTERM\u{001B}[m env var\n\
     is \u{001B}[01mdumb\u{001B}[m or undefined.\n\
     item-end\n";
  complete ["birds"; "fly"; "--"; "--__complete="] @@ __POS_OF__
    "1\n";
  ()

let test_opts_starts =
  Test.test "complete optional argument names" @@ fun () ->
  complete ["birds"; "--__complete=-"] @@ __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     -k\n\
     Kind of entity\n\
     item-end\n\
     item\n\
     --kind\n\
     Kind of entity\n\
     item-end\n\
     item\n\
     --can-fly\n\
     \u{001B}[04mBOOL\u{001B}[m indicates if the entity can fly.\n\
     item-end\n\
     item\n\
     --version\n\
     Show version information.\n\
     item-end\n\
     item\n\
     --help\n\
     Show this help in format \u{001B}[04mFMT\u{001B}[m. The value \u{001B}[04mFMT\u{001B}[m must be one of \u{001B}[01mauto\u{001B}[m, \u{001B}[01mpager\u{001B}[m, \u{001B}[01mgroff\u{001B}[m\n\
     or \u{001B}[01mplain\u{001B}[m. With \u{001B}[01mauto\u{001B}[m, the format is \u{001B}[01mpager\u{001B}[m or \u{001B}[01mplain\u{001B}[m whenever the \u{001B}[01mTERM\u{001B}[m env var\n\
     is \u{001B}[01mdumb\u{001B}[m or undefined.\n\
     item-end\n\
     group\n\
     Subcommands\n";
  complete ["birds"; "--__complete=--"] @@ __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     --kind\n\
     Kind of entity\n\
     item-end\n\
     item\n\
     --can-fly\n\
     \u{001B}[04mBOOL\u{001B}[m indicates if the entity can fly.\n\
     item-end\n\
     item\n\
     --version\n\
     Show version information.\n\
     item-end\n\
     item\n\
     --help\n\
     Show this help in format \u{001B}[04mFMT\u{001B}[m. The value \u{001B}[04mFMT\u{001B}[m must be one of \u{001B}[01mauto\u{001B}[m, \u{001B}[01mpager\u{001B}[m, \u{001B}[01mgroff\u{001B}[m\n\
     or \u{001B}[01mplain\u{001B}[m. With \u{001B}[01mauto\u{001B}[m, the format is \u{001B}[01mpager\u{001B}[m or \u{001B}[01mplain\u{001B}[m whenever the \u{001B}[01mTERM\u{001B}[m env var\n\
     is \u{001B}[01mdumb\u{001B}[m or undefined.\n\
     item-end\n";
  ()

let test_opt_value =
  Test.test "complete optional argument values" @@ fun () ->
  (* Glued *)
  complete ["birds"; "--__complete=--can-fly="] @@ __POS_OF__
    "1\n\
     group\n\
     Values\n\
     item\n\
     true\n\
     \n\
     item-end\n\
     item\n\
     false\n\
     \n\
     item-end\n";
  (* next token *)
  complete ["birds"; "--can-fly"; "--__complete="] @@ __POS_OF__
    "1\n\
     group\n\
     Values\n\
     item\n\
     true\n\
     \n\
     item-end\n\
     item\n\
     false\n\
     \n\
     item-end\n";
  complete ["birds"; "--can-fly=true"; "--__complete="] @@ __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     -k\n\
     Kind of entity\n\
     item-end\n\
     item\n\
     --kind\n\
     Kind of entity\n\
     item-end\n\
     item\n\
     --can-fly\n\
     \u{001B}[04mBOOL\u{001B}[m indicates if the entity can fly.\n\
     item-end\n\
     item\n\
     --version\n\
     Show version information.\n\
     item-end\n\
     item\n\
     --help\n\
     Show this help in format \u{001B}[04mFMT\u{001B}[m. The value \u{001B}[04mFMT\u{001B}[m must be one of \u{001B}[01mauto\u{001B}[m, \u{001B}[01mpager\u{001B}[m, \u{001B}[01mgroff\u{001B}[m\n\
     or \u{001B}[01mplain\u{001B}[m. With \u{001B}[01mauto\u{001B}[m, the format is \u{001B}[01mpager\u{001B}[m or \u{001B}[01mplain\u{001B}[m whenever the \u{001B}[01mTERM\u{001B}[m env var\n\
     is \u{001B}[01mdumb\u{001B}[m or undefined.\n\
     item-end\n";
  complete ["birds"; "--can-fly"; "true"; "--__complete="] @@ __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     -k\n\
     Kind of entity\n\
     item-end\n\
     item\n\
     --kind\n\
     Kind of entity\n\
     item-end\n\
     item\n\
     --can-fly\n\
     \u{001B}[04mBOOL\u{001B}[m indicates if the entity can fly.\n\
     item-end\n\
     item\n\
     --version\n\
     Show version information.\n\
     item-end\n\
     item\n\
     --help\n\
     Show this help in format \u{001B}[04mFMT\u{001B}[m. The value \u{001B}[04mFMT\u{001B}[m must be one of \u{001B}[01mauto\u{001B}[m, \u{001B}[01mpager\u{001B}[m, \u{001B}[01mgroff\u{001B}[m\n\
     or \u{001B}[01mplain\u{001B}[m. With \u{001B}[01mauto\u{001B}[m, the format is \u{001B}[01mpager\u{001B}[m or \u{001B}[01mplain\u{001B}[m whenever the \u{001B}[01mTERM\u{001B}[m env var\n\
     is \u{001B}[01mdumb\u{001B}[m or undefined.\n\
     item-end\n";
  ()

let test_context_sensitive =
  Test.test "context sensitive completions" @@ fun () ->
  complete ["lookup"; "--__complete="] @@ __POS_OF__
    "1\n\
     group\n\
     Values\n\
     item\n\
     sparrow\n\
     \n\
     item-end\n\
     item\n\
     parrot\n\
     \n\
     item-end\n\
     item\n\
     pigeon\n\
     \n\
     item-end\n\
     item\n\
     salmon\n\
     \n\
     item-end\n\
     item\n\
     trout\n\
     \n\
     item-end\n\
     item\n\
     piranha\n\
     \n\
     item-end\n\
     group\n\
     Options\n\
     item\n\
     -k\n\
     \u{001B}[04mENUM\u{001B}[m restricts the animal kind. Must be either \u{001B}[01mbird\u{001B}[m or \u{001B}[01mfish\u{001B}[m\n\
     item-end\n\
     item\n\
     --kind\n\
     \u{001B}[04mENUM\u{001B}[m restricts the animal kind. Must be either \u{001B}[01mbird\u{001B}[m or \u{001B}[01mfish\u{001B}[m\n\
     item-end\n\
     item\n\
     --version\n\
     Show version information.\n\
     item-end\n\
     item\n\
     --help\n\
     Show this help in format \u{001B}[04mFMT\u{001B}[m. The value \u{001B}[04mFMT\u{001B}[m must be one of \u{001B}[01mauto\u{001B}[m, \u{001B}[01mpager\u{001B}[m, \u{001B}[01mgroff\u{001B}[m\n\
     or \u{001B}[01mplain\u{001B}[m. With \u{001B}[01mauto\u{001B}[m, the format is \u{001B}[01mpager\u{001B}[m or \u{001B}[01mplain\u{001B}[m whenever the \u{001B}[01mTERM\u{001B}[m env var\n\
     is \u{001B}[01mdumb\u{001B}[m or undefined.\n\
     item-end\n";
  complete ["lookup"; "-kfish"; "--__complete=s"] @@ __POS_OF__
    "1\n\
     group\n\
     Values\n\
     item\n\
     salmon\n\
     \n\
     item-end\n";
  complete ["lookup"; "-kbird"; "--__complete=p"] @@ __POS_OF__
    "1\n\
     group\n\
     Values\n\
     item\n\
     parrot\n\
     \n\
     item-end\n\
     item\n\
     pigeon\n\
     \n\
     item-end\n";
  ()

let test_restart_restricted_tool =
  Test.test "restart restricted tool" @@ fun () ->
  let cmd =
    Cmd.make (Cmd.info "test_restart_restricted") @@
    let+ verb = Arg.(value & flag & info ["verbose"])
    and+ tool =
      let tool = Arg.enum ~docv:"VCS" ["git", `Git; "hg", `Hg] in
      Arg.(required & pos 0 (some tool) None & info [])
    and+ args =
      let arg =
        let completion = Arg.Completion.complete_restart in
        Arg.Conv.of_conv Arg.string ~docv:"ARG" ~completion
      in
      Arg.(value & pos_right 0 arg [] & info [])
    in
    ()
  in
  let complete = Testing_cmdliner.snap_completion cmd in
  complete ["--__complete="] @@ __POS_OF__
    "1\n\
     group\n\
     Values\n\
     item\n\
     git\n\
     \n\
     item-end\n\
     item\n\
     hg\n\
     \n\
     item-end\n\
     group\n\
     Options\n\
     item\n\
     --verbose\n\
     \n\
     item-end\n\
     item\n\
     --help\n\
     Show this help in format \u{001B}[04mFMT\u{001B}[m. The value \u{001B}[04mFMT\u{001B}[m must be one of \u{001B}[01mauto\u{001B}[m, \u{001B}[01mpager\u{001B}[m, \u{001B}[01mgroff\u{001B}[m\n\
     or \u{001B}[01mplain\u{001B}[m. With \u{001B}[01mauto\u{001B}[m, the format is \u{001B}[01mpager\u{001B}[m or \u{001B}[01mplain\u{001B}[m whenever the \u{001B}[01mTERM\u{001B}[m env var\n\
     is \u{001B}[01mdumb\u{001B}[m or undefined.\n\
     item-end\n";
  complete ["--__complete=g"] @@ __POS_OF__
    "1\n\
     group\n\
     Values\n\
     item\n\
     git\n\
     \n\
     item-end\n";
  (* Note no reset here: as there is no -- token *)
  complete ["git"; "--__complete="] @@ __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     --verbose\n\
     \n\
     item-end\n\
     item\n\
     --help\n\
     Show this help in format \u{001B}[04mFMT\u{001B}[m. The value \u{001B}[04mFMT\u{001B}[m must be one of \u{001B}[01mauto\u{001B}[m, \u{001B}[01mpager\u{001B}[m, \u{001B}[01mgroff\u{001B}[m\n\
     or \u{001B}[01mplain\u{001B}[m. With \u{001B}[01mauto\u{001B}[m, the format is \u{001B}[01mpager\u{001B}[m or \u{001B}[01mplain\u{001B}[m whenever the \u{001B}[01mTERM\u{001B}[m env var\n\
     is \u{001B}[01mdumb\u{001B}[m or undefined.\n\
     item-end\n";
  complete ["--"; "git"; "--__complete="] @@ __POS_OF__
    "1\n\
     restart\n";
  ()

let test_restart_any_tool =
  Test.test "restart any tool" @@ fun () ->
  let cmd =
    Cmd.make (Cmd.info "test_restart") @@
    let arg ~docv =
      let completion = Arg.Completion.complete_restart in
      Arg.Conv.of_conv Arg.string ~docv:"TOOL" ~completion
    in
    let+ verb = Arg.(value & flag & info ["verbose"])
    and+ tool = Arg.(required & pos 0 (some (arg ~docv:"TOOL")) None & info [])
    and+ args = Arg.(value & pos_right 0 (arg ~docv:"ARG") [] & info []) in
    ()
  in
  let complete = Testing_cmdliner.snap_completion cmd in
  complete ["--__complete="] @@ __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     --verbose\n\
     \n\
     item-end\n\
     item\n\
     --help\n\
     Show this help in format \u{001B}[04mFMT\u{001B}[m. The value \u{001B}[04mFMT\u{001B}[m must be one of \u{001B}[01mauto\u{001B}[m, \u{001B}[01mpager\u{001B}[m, \u{001B}[01mgroff\u{001B}[m\n\
     or \u{001B}[01mplain\u{001B}[m. With \u{001B}[01mauto\u{001B}[m, the format is \u{001B}[01mpager\u{001B}[m or \u{001B}[01mplain\u{001B}[m whenever the \u{001B}[01mTERM\u{001B}[m env var\n\
     is \u{001B}[01mdumb\u{001B}[m or undefined.\n\
     item-end\n";
  (* The following two do not restart because -- is missing *)
  complete ["--__complete=gi"] @@ __POS_OF__
    "1\n";
  complete ["git"; "--__complete="] @@ __POS_OF__
    "1\n\
     group\n\
     Options\n\
     item\n\
     --verbose\n\
     \n\
     item-end\n\
     item\n\
     --help\n\
     Show this help in format \u{001B}[04mFMT\u{001B}[m. The value \u{001B}[04mFMT\u{001B}[m must be one of \u{001B}[01mauto\u{001B}[m, \u{001B}[01mpager\u{001B}[m, \u{001B}[01mgroff\u{001B}[m\n\
     or \u{001B}[01mplain\u{001B}[m. With \u{001B}[01mauto\u{001B}[m, the format is \u{001B}[01mpager\u{001B}[m or \u{001B}[01mplain\u{001B}[m whenever the \u{001B}[01mTERM\u{001B}[m env var\n\
     is \u{001B}[01mdumb\u{001B}[m or undefined.\n\
     item-end\n";
  (* These must restart *)
  complete ["--"; "--__complete=gi"] @@ __POS_OF__
    "1\n\
     restart\n";
  complete ["--"; "git"; "--__complete="] @@ __POS_OF__
    "1\n\
     restart\n";
  ()

let test_context =
  Test.test "Context sensitive completion from optional argument" @@ fun () ->
  let ctx =
    Arg.(value & opt (some bool) None & info ["ctx"])
  in
  let dep =
    let complete ctx ~token:_ = match ctx with
    | None -> Ok [Arg.Completion.string "ctx-parse-error"]
    | Some None -> Ok [Arg.Completion.string "no-context"]
    | Some (Some ctx) -> Ok [Arg.Completion.string (Bool.to_string ctx)]
    in
    let completion = Arg.Completion.make ~context:ctx complete in
    Arg.Conv.of_conv Arg.string ~docv:"SPECIAL" ~completion
  in
  let () = (* test [dep] converter on an option *)
    let cmd =
      Cmd.make (Cmd.info "test_context") @@
      let+ lookup = Arg.(value & opt dep "nothing" & info ["dep"])
      and+ ctx in
      ()
    in
    let complete = Testing_cmdliner.snap_completion cmd in
    complete ["--dep"; "--__complete="] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       no-context\n\
       \n\
       item-end\n";
    complete ["--ctx=hey"; "--dep"; "--__complete="] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       ctx-parse-error\n\
       \n\
       item-end\n";
    complete ["--ctx=true"; "--dep"; "--__complete="] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       true\n\
       \n\
       item-end\n";
    complete ["--dep"; "--__complete="; "--ctx=true"] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       true\n\
       \n\
       item-end\n";
  in
  let () =
    let cmd =
      Cmd.make (Cmd.info "test_context") @@
      let+ lookup = Arg.(value & pos 0 dep "nothing" & info [])
      and+ ctx in
      ()
    in
    let complete = Testing_cmdliner.snap_completion cmd in
    complete ["--__complete=a"] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       no-context\n\
       \n\
       item-end\n";
    complete ["--ctx=hey"; "--__complete=a"] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       ctx-parse-error\n\
       \n\
       item-end\n";
    complete ["--ctx=true"; "--__complete=a"] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       true\n\
       \n\
       item-end\n";
    complete ["--__complete=a"; "--ctx=true"] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       true\n\
       \n\
       item-end\n";
  in
  ()

let test_context =
  Test.test "Context sensitive completion from positional argument" @@ fun () ->
  let ctx0 = Arg.(value & pos 0 (some bool) None & info []) in
  let dep =
    let complete ctx ~token:_ = match ctx with
    | None -> Ok [Arg.Completion.string "ctx-parse-error"]
    | Some None -> Ok [Arg.Completion.string "no-context"]
    | Some (Some ctx) -> Ok [Arg.Completion.string (Bool.to_string ctx)]
    in
    let completion = Arg.Completion.make ~context:ctx0 complete in
    Arg.Conv.of_conv Arg.string ~docv:"SPECIAL" ~completion
  in
  let () = (* test [dep] converter on an option *)
    let cmd =
      Cmd.make (Cmd.info "test_context") @@
      let+ lookup = Arg.(value & opt dep "nothing" & info ["dep"])
      and+ ctx0 in
      ()
    in
    let complete = Testing_cmdliner.snap_completion cmd in
    complete ["--dep"; "--__complete="] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       no-context\n\
       \n\
       item-end\n";
    complete ["bla"; "--dep"; "--__complete="] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       ctx-parse-error\n\
       \n\
       item-end\n";
    complete ["true"; "--dep"; "--__complete="] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       true\n\
       \n\
       item-end\n";
    complete ["--dep"; "--__complete="; "true"] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       true\n\
       \n\
       item-end\n";
  in
  let () =
    let cmd =
      Cmd.make (Cmd.info "test_context") @@
      let+ lookup = Arg.(value & pos 1 dep "nothing" & info [])
      and+ ctx0 in
      ()
    in
    let complete = Testing_cmdliner.snap_completion cmd in
    complete ["hey"; "--__complete=a"] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       ctx-parse-error\n\
       \n\
       item-end\n";
    complete ["true"; "--__complete=a"] @@ __POS_OF__
      "1\n\
       group\n\
       Values\n\
       item\n\
       true\n\
       \n\
       item-end\n";
  in
  ()

let main () =
  let doc = "Test completion" in
  Test.main ~doc @@ fun () -> Test.autorun ()

let () = if !Sys.interactive then () else exit (main ())
