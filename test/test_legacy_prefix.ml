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
"test_group: \x1B[31munknown\x1B[m command \x1B[01mbir\x1B[m. Did you mean \x1B[01mbirds\x1B[m?
Usage: \x1B[01mtest_group\x1B[m \x1B[04mCOMMAND\x1B[m …
Try \x1B[01mtest_group --help\x1B[m for more information.
";
  error_nolegacy `Term ["birds"; "fl"] @@ __POS_OF__
"test_group: \x1B[31munknown\x1B[m command \x1B[01mfl\x1B[m. Did you mean \x1B[01mfly\x1B[m?
Usage: \x1B[01mtest_group birds\x1B[m [\x1B[04mCOMMAND\x1B[m] …
Try \x1B[01mtest_group birds --help\x1B[m or \x1B[01mtest_group --help\x1B[m for more information.
";
  error_nolegacy `Term ["mam"] @@ __POS_OF__
"test_group: \x1B[31munknown\x1B[m command \x1B[01mmam\x1B[m. Must be one of \x1B[01mbirds\x1B[m, \x1B[01mcamels\x1B[m, \x1B[01mfishs\x1B[m or \x1B[01mmammals\x1B[m
Usage: \x1B[01mtest_group\x1B[m \x1B[04mCOMMAND\x1B[m …
Try \x1B[01mtest_group --help\x1B[m for more information.
";
  ()

let test_cmd =
  Test.test "option names" @@ fun () ->
  parse_legacy ["birds"; "fly"; "--sp"; "3"] @@ __POS_OF__ ();
  (**)
  error_nolegacy `Term ["birds"; "fly"; "--sp"; ] @@ __POS_OF__
"test_group: \x1B[31munknown\x1B[m option \x1B[01m--sp\x1B[m
Usage: \x1B[01mtest_group birds fly\x1B[m [\x1B[01m--speed\x1B[m=\x1B[04mSPEED\x1B[m] [\x1B[04mOPTION\x1B[m]… [\x1B[04mBIRD\x1B[m]
Try \x1B[01mtest_group birds fly --help\x1B[m or \x1B[01mtest_group --help\x1B[m for more information.
";
  error_nolegacy `Term ["birds"; "fly"; "--spe"; ] @@ __POS_OF__
"test_group: \x1B[31munknown\x1B[m option \x1B[01m--spe\x1B[m. Did you mean \x1B[01m--speed\x1B[m?
Usage: \x1B[01mtest_group birds fly\x1B[m [\x1B[01m--speed\x1B[m=\x1B[04mSPEED\x1B[m] [\x1B[04mOPTION\x1B[m]… [\x1B[04mBIRD\x1B[m]
Try \x1B[01mtest_group birds fly --help\x1B[m or \x1B[01mtest_group --help\x1B[m for more information.
";
  ()

let main () =
  let doc = "Test CMDLINER_LEGACY_PREFIXES behaviour" in
  Test.main ~doc @@ fun () -> Test.autorun ()

let () = if !Sys.interactive then () else exit (main ())
