(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Cmdliner
open Cmdliner.Term.Syntax

let env ~legacy_prefixes:b =
  let b = string_of_bool b in
  function
  | "CMDLINER_LEGACY_PREFIXES" -> Some b
  | var -> Sys.getenv_opt var

let legacy = env ~legacy_prefixes:true
let nolegacy = env ~legacy_prefixes:false

let cmd = Testing_cmdliner.sample_group_cmd
let parse_legacy = Testing_cmdliner.snap_parse ~env:legacy Test.T.unit cmd
let error_nolegacy err = Testing_cmdliner.snap_eval_error ~env:nolegacy err cmd

(* Note, we don't test Arg.conv since we cannot control it through eval's
   env variable. *)

(* The tests have the following structure:

   let test =
     (* A few snapshots of valid cli parses *)
     parse …
     (* A few snapshots of invalid cli parses *)
     error … *)

let test_cmd =
  Test.test "command names" @@ fun () ->
  parse_legacy ["bir"] @@ __POS_OF__ ();
  parse_legacy ["bir"; "fly"] @@ __POS_OF__ ();
  parse_legacy ["mamma"] @@ __POS_OF__ ();
  (**)
  error_nolegacy `Term ["bir"] @@ __POS_OF__
"Usage: \u{001B}[01mtest_group\u{001B}[m [\u{001B}[01m--help\u{001B}[m] \u{001B}[04mCOMMAND\u{001B}[m …\n\
test_group: \u{001B}[31munknown\u{001B}[m command \u{001B}[01mbir\u{001B}[m. Did you mean \u{001B}[01mbirds\u{001B}[m?\n";
  error_nolegacy `Term ["birds"; "fl"] @@ __POS_OF__
"Usage: \u{001B}[01mtest_group birds\u{001B}[m [\u{001B}[01m--help\u{001B}[m] [\u{001B}[04mCOMMAND\u{001B}[m] …\n\
test_group: \u{001B}[31munknown\u{001B}[m command \u{001B}[01mfl\u{001B}[m. Did you mean \u{001B}[01mfly\u{001B}[m?\n";
  error_nolegacy `Term ["mam"] @@ __POS_OF__ "Usage: \u{001B}[01mtest_group\u{001B}[m [\u{001B}[01m--help\u{001B}[m] \u{001B}[04mCOMMAND\u{001B}[m …\n\
                                              test_group: \u{001B}[31munknown\u{001B}[m command \u{001B}[01mmam\u{001B}[m. Must be one of \u{001B}[01mbirds\u{001B}[m, \u{001B}[01mcamels\u{001B}[m, \u{001B}[01mfishs\u{001B}[m or\n\
                                             \            \u{001B}[01mmammals\u{001B}[m\n";
  ()

let test_cmd =
  Test.test "option names" @@ fun () ->
  parse_legacy ["birds"; "fly"; "--sp"; "3"] @@ __POS_OF__ ();
  (**)
  error_nolegacy `Term ["birds"; "fly"; "--sp"; ] @@ __POS_OF__
"Usage: \u{001B}[01mtest_group birds fly\u{001B}[m [\u{001B}[01m--help\u{001B}[m] [\u{001B}[01m--speed\u{001B}[m=\u{001B}[04mSPEED\u{001B}[m] [\u{001B}[04mOPTION\u{001B}[m]… [\u{001B}[04mBIRD\u{001B}[m]\n\
test_group: \u{001B}[31munknown\u{001B}[m option \u{001B}[01m--sp\u{001B}[m\n";
  error_nolegacy `Term ["birds"; "fly"; "--spe"; ] @@ __POS_OF__
"Usage: \u{001B}[01mtest_group birds fly\u{001B}[m [\u{001B}[01m--help\u{001B}[m] [\u{001B}[01m--speed\u{001B}[m=\u{001B}[04mSPEED\u{001B}[m] [\u{001B}[04mOPTION\u{001B}[m]… [\u{001B}[04mBIRD\u{001B}[m]\n\
test_group: \u{001B}[31munknown\u{001B}[m option \u{001B}[01m--spe\u{001B}[m. Did you mean \u{001B}[01m--speed\u{001B}[m?\n";
  ()

let main () =
  let doc = "Test CMDLINER_LEGACY_PREFIXES behaviour" in
  Test.main ~doc @@ fun () -> Test.autorun ()

let () = if !Sys.interactive then () else exit (main ())
