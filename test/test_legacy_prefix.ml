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
{|test_group: unknown command 'bir', did you mean 'birds'?
Usage: test_group COMMAND …
Try 'test_group --help' for more information.
|};
  error_nolegacy `Term ["birds"; "fl"] @@ __POS_OF__
{|test_group: unknown command 'fl', did you mean 'fly'?
Usage: test_group birds [COMMAND] …
Try 'test_group birds --help' or 'test_group --help' for more information.
|};
  error_nolegacy `Term ["mam"] @@ __POS_OF__
{|test_group: unknown command 'mam', must be one of 'birds', 'camels', 'fishs' or 'mammals'.
Usage: test_group COMMAND …
Try 'test_group --help' for more information.
|};
  ()

let test_cmd =
  Test.test "option names" @@ fun () ->
  parse_legacy ["birds"; "fly"; "--sp"; "3"] @@ __POS_OF__ ();
  (**)
  error_nolegacy `Term ["birds"; "fly"; "--sp"; ] @@ __POS_OF__
    {|test_group: unknown option '--sp'.
Usage: test_group birds fly [--speed=SPEED] [OPTION]… [BIRD]
Try 'test_group birds fly --help' or 'test_group --help' for more information.
|};
  error_nolegacy `Term ["birds"; "fly"; "--spe"; ] @@ __POS_OF__
{|test_group: unknown option '--spe', did you mean '--speed'?
Usage: test_group birds fly [--speed=SPEED] [OPTION]… [BIRD]
Try 'test_group birds fly --help' or 'test_group --help' for more information.
|};
  ()

let main () =
  let doc = "Test CMDLINER_LEGACY_PREFIXES behaviour" in
  Test.main ~doc @@ fun () -> Test.autorun ()

let () = if !Sys.interactive then () else exit (main ())
