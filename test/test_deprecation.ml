(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Cmdliner
open Cmdliner.Term.Syntax

let cmd = Testing_cmdliner.sample_group_cmd
let warning ?env = Testing_cmdliner.snap_parse_warnings ?env cmd
let test_env = function "BACTRIAN" -> Some "true" | var -> Sys.getenv_opt var

let deprecated_command =
  Test.test "Deprecated command" @@ fun () ->
  warning ["camels"] @@ __POS_OF__
    "test_group: command \u{001B}[01mcamels\u{001B}[m: deprecated, use 'mammals' instead.\n";
  ()

let deprecated_arg =
  Test.test "Deprecated option argument" @@ fun () ->
  warning ["camels"; "-b"] @@ __POS_OF__
    "test_group: command \u{001B}[01mcamels\u{001B}[m: deprecated, use 'mammals' instead.\n\
    \            option \u{001B}[01m-b\u{001B}[m: deprecated, use nothing instead.\n";
  warning ["camels"; "--bactrian"] @@ __POS_OF__
    "test_group: command \u{001B}[01mcamels\u{001B}[m: deprecated, use 'mammals' instead.\n\
    \            option \u{001B}[01m--bactrian\u{001B}[m: deprecated, use nothing instead.\n";
  ()

let deprecated_pos =
  Test.test "Deprecated positional argument" @@ fun () ->
  warning ["camels"; "bla"] @@ __POS_OF__
    "test_group: command \u{001B}[01mcamels\u{001B}[m: deprecated, use 'mammals' instead.\n\
    \            argument \u{001B}[01mbla\u{001B}[m: deprecated, herds are ignored.\n";
  ()

let deprecated_env =
  Test.test "Deprecated env variable" @@ fun () ->
  warning ~env:test_env ["camels"] @@ __POS_OF__
    "test_group: command \u{001B}[01mcamels\u{001B}[m: deprecated, use 'mammals' instead.\n\
    \            environment variable \u{001B}[01mBACTRIAN\u{001B}[m: deprecated, use nothing instead.\n";
  warning ~env:test_env ["camels"; "-b"] (* takes over env *) @@ __POS_OF__
    "test_group: command \u{001B}[01mcamels\u{001B}[m: deprecated, use 'mammals' instead.\n\
    \            option \u{001B}[01m-b\u{001B}[m: deprecated, use nothing instead.\n";
  ()

let deprecated_combined =
  Test.test "Deprecation combined" @@ fun () ->
  warning ~env:test_env ["camels"; "bla"; ] @@ __POS_OF__
    "test_group: command \u{001B}[01mcamels\u{001B}[m: deprecated, use 'mammals' instead.\n\
    \            argument \u{001B}[01mbla\u{001B}[m: deprecated, herds are ignored.\n\
    \            environment variable \u{001B}[01mBACTRIAN\u{001B}[m: deprecated, use nothing instead.\n";
  ()

let main () =
  let doc = "Test deprecation messages" in
  Test.main ~doc @@ fun () -> Test.autorun ()

let () = if !Sys.interactive then () else exit (main ())
