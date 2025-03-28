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
     (* A few snapshots of valid cli parses *)
     parse …
     (* A few snapshots of invalid cli parses *)
     error …
     (* A snapshot of a plain text version of the manual *)
     Testing_cmdliner.snap_man …  *)

(* Positional arguments *)

let test_pos_all =
  Test.test "Arg.pos_all" @@ fun () ->
  let cmd =
    Cmd.make (Cmd.info "test_pos_all" ~doc:"Test pos all") @@
    let+ all = Arg.(value & pos_all string [] & info [] ~docv:"THEARG") in
    all
  in
  let parse = Testing_cmdliner.snap_parse Test.T.(list string) cmd in
  let error err = Testing_cmdliner.snap_eval_error err cmd in
  parse [] @@ __POS_OF__ [];
  parse ["0"] @@ __POS_OF__ ["0"];
  parse ["--"; "0"] @@ __POS_OF__ ["0"];
  parse ["0";"1"] @@ __POS_OF__ ["0"; "1"];
  parse ["0";"--"; "1"] @@ __POS_OF__ ["0"; "1"];
  (**)
  error `Term ["--opt"] @@ __POS_OF__
  "test_pos_all: \x1B[31munknown\x1B[m option \x1B[01m--opt\x1B[m
Usage: \x1B[01mtest_pos_all\x1B[m [\x1B[04mOPTION\x1B[m]… [\x1B[04mTHEARG\x1B[m]…
Try \x1B[01mtest_pos_all --help\x1B[m for more information.
";
  (**)
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
{|NAME
       test_pos_all - Test pos all

SYNOPSIS
       test_pos_all [OPTION]… [THEARG]…

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

EXIT STATUS
       test_pos_all exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.
|};
  ()

let test_pos_left =
  Test.test "Arg.pos_left" @@ fun () ->
  let cmd =
    Cmd.make (Cmd.info "test_pos_left" ~doc:"Test pos left") @@
    let+ left = Arg.(value & pos_left 2 string [] & info [] ~docv:"LEFT") in
    left
  in
  let parse = Testing_cmdliner.snap_parse Test.T.(list string) cmd in
  let error err = Testing_cmdliner.snap_eval_error err cmd in
  parse [] @@ __POS_OF__ [];
  parse ["--"] @@ __POS_OF__ [];
  parse ["0"] @@ __POS_OF__ ["0"];
  parse ["0"; "--"; "1" ] @@ __POS_OF__ ["0"; "1"];
  parse ["0"; "1" ] @@ __POS_OF__ ["0"; "1"];
  (**)
  error `Term ["0"; "1"; "2"] @@ __POS_OF__
  "test_pos_left: \x1B[31mtoo many arguments\x1B[m, don't know what to do with \x1B[01m2\x1B[m
Usage: \x1B[01mtest_pos_left\x1B[m [\x1B[04mOPTION\x1B[m]… [\x1B[04mLEFT\x1B[m] [\x1B[04mLEFT\x1B[m]
Try \x1B[01mtest_pos_left --help\x1B[m for more information.
";
  (**)
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
{|NAME
       test_pos_left - Test pos left

SYNOPSIS
       test_pos_left [OPTION]… [LEFT] [LEFT]

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

EXIT STATUS
       test_pos_left exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.
|};
  ()

let test_pos_req =
  Test.test "Arg.required & Arg.pos" @@ fun () ->
  let cmd =
    Cmd.make (Cmd.info "test_pos_req" ~doc:"Test pos req arguments") @@
    let+ r1 = Arg.(required & pos 0 (some string) None & info [] ~docv:"R1")
    and+ r2 = Arg.(required & pos 1 (some string) None & info [] ~docv:"R2")
    and+ r3 = Arg.(required & pos 2 (some string) None & info [] ~docv:"R3")
    and+ right =
      Arg.(non_empty & pos_right 2 string [] & info [] ~docv:"RIGHT")
    in
    r1, r2, r3, right
  in
  let t = Test.T.(t4 string string string (list string)) in
  let parse = Testing_cmdliner.snap_parse t cmd in
  parse ["r1"; "r2"; "r3"; "r4"] @@ __POS_OF__ ("r1", "r2", "r3", ["r4"]);
  parse ["r1"; "r2"; "r3"; "r4"; "r5"] @@ __POS_OF__
    ("r1", "r2", "r3", ["r4"; "r5"]);
  (**)
  let error = Testing_cmdliner.snap_eval_error `Term cmd in
  error [] @@ __POS_OF__
"test_pos_req: required arguments \x1B[04mR1\x1B[m, \x1B[04mR2\x1B[m, \x1B[04mR3\x1B[m, \x1B[04mRIGHT\x1B[m are \x1B[31mmissing\x1B[m
Usage: \x1B[01mtest_pos_req\x1B[m [\x1B[04mOPTION\x1B[m]… \x1B[04mR1\x1B[m \x1B[04mR2\x1B[m \x1B[04mR3\x1B[m \x1B[04mRIGHT\x1B[m…
Try \x1B[01mtest_pos_req --help\x1B[m for more information.
";
  error ["r1"] @@ __POS_OF__
"test_pos_req: required arguments \x1B[04mR2\x1B[m, \x1B[04mR3\x1B[m, \x1B[04mRIGHT\x1B[m are \x1B[31mmissing\x1B[m
Usage: \x1B[01mtest_pos_req\x1B[m [\x1B[04mOPTION\x1B[m]… \x1B[04mR1\x1B[m \x1B[04mR2\x1B[m \x1B[04mR3\x1B[m \x1B[04mRIGHT\x1B[m…
Try \x1B[01mtest_pos_req --help\x1B[m for more information.
";
  error ["r1"; "r2"] @@ __POS_OF__
"test_pos_req: required arguments \x1B[04mR3\x1B[m, \x1B[04mRIGHT\x1B[m are \x1B[31mmissing\x1B[m
Usage: \x1B[01mtest_pos_req\x1B[m [\x1B[04mOPTION\x1B[m]… \x1B[04mR1\x1B[m \x1B[04mR2\x1B[m \x1B[04mR3\x1B[m \x1B[04mRIGHT\x1B[m…
Try \x1B[01mtest_pos_req --help\x1B[m for more information.
";
  error ["r1"; "r2"; "r3"] @@ __POS_OF__
"test_pos_req: required argument \x1B[04mRIGHT\x1B[m is \x1B[31mmissing\x1B[m
Usage: \x1B[01mtest_pos_req\x1B[m [\x1B[04mOPTION\x1B[m]… \x1B[04mR1\x1B[m \x1B[04mR2\x1B[m \x1B[04mR3\x1B[m \x1B[04mRIGHT\x1B[m…
Try \x1B[01mtest_pos_req --help\x1B[m for more information.
";
  (**)
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
{|NAME
       test_pos_req - Test pos req arguments

SYNOPSIS
       test_pos_req [OPTION]… R1 R2 R3 RIGHT…

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

EXIT STATUS
       test_pos_req exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.
|};
  ()

let test_pos_left_right =
  Test.test "Arg.pos_{left,right}" @@ fun () ->
  let cmd =
    Cmd.make (Cmd.info "test_pos" ~doc:"Test pos arguments") @@
    let+ l = Arg.(value & pos_left 2 string [] & info [] ~docv:"LEFT")
    and+ t = Arg.(value & pos 2 string "undefined" & info [] ~docv:"TWO")
    and+ r = Arg.(value & pos_right 2 string [] & info [] ~docv:"RIGHT") in
    (l, t, r)
  in
  let t = Test.T.(t3 (list string) string (list string)) in
  let snap = Testing_cmdliner.snap_parse t cmd in
  snap [] @@ __POS_OF__ ([], "undefined", []);
  snap ["0"] @@ __POS_OF__ (["0"], "undefined", []);
  snap ["0"; "1"] @@ __POS_OF__ (["0"; "1"], "undefined", []);
  snap ["0"; "1"; "2"] @@ __POS_OF__ (["0"; "1"], "2", []);
  snap ["0"; "1"; "2"; "3"] @@ __POS_OF__ (["0"; "1"], "2", ["3"]);
  snap ["0"; "1"; "2"; "3"; "4"] @@ __POS_OF__ (["0"; "1"], "2", ["3"; "4"]);
  (**)
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
{|NAME
       test_pos - Test pos arguments

SYNOPSIS
       test_pos [OPTION]… [LEFT] [LEFT] [TWO] [RIGHT]…

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

EXIT STATUS
       test_pos exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.
|}
;
()

let test_pos_left_right_rev =
  Test.test "Arg.pos_{left,right} ~rev:true" @@ fun () ->
  let cmd =
    Cmd.make (Cmd.info "test_pos" ~doc:"Test pos arguments") @@
    let rev = true in
    let+ l = Arg.(value & pos_left 2 ~rev string [] & info [] ~docv:"LEFT")
    and+ t = Arg.(value & pos 2 ~rev string "undefined" & info [] ~docv:"TWO")
    and+ r = Arg.(value & pos_right 2 ~rev string [] & info [] ~docv:"RIGHT") in
    (l, t, r)
  in
  let t = Test.T.(t3 (list string) string (list string)) in
  let snap = Testing_cmdliner.snap_parse t cmd in
  snap [] @@ __POS_OF__ ([], "undefined", []);
  snap ["0"] @@ __POS_OF__ ([], "undefined", ["0"]);
  snap ["0"; "1"] @@ __POS_OF__ ([], "undefined", ["0"; "1"]);
  snap ["0"; "1"; "2"] @@ __POS_OF__ ([], "0", ["1"; "2"]);
  snap ["0"; "1"; "2"; "3"] @@ __POS_OF__ (["0"], "1", ["2"; "3"]);
  snap ["0"; "1"; "2"; "3"; "4"] @@ __POS_OF__ (["0"; "1"], "2", ["3"; "4"]);
  snap ["0"; "1"; "2"; "3"; "4"; "5"] @@ __POS_OF__
    (["0"; "1"; "2"], "3", ["4"; "5"]);
  (**)
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
{|NAME
       test_pos - Test pos arguments

SYNOPSIS
       test_pos [OPTION]… [LEFT]… [TWO] [RIGHT] [RIGHT]

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

EXIT STATUS
       test_pos exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.
|};
  ()

(* Optional arguments *)

let test_opt_required =
  Test.test "Arg.required & Arg.opt" @@ fun () ->
  let cmd =
    let doc = "Test optional required arguments (don't do this)" in
    Cmd.make (Cmd.info "test_opt_req" ~doc) @@
    let+ req =
      Arg.(required & opt (some string) None & info ["r"; "req"] ~docv:"ARG")
    in
    req
  in
  let snap = Testing_cmdliner.snap_parse Test.T.string cmd in
  snap ["-ra"] @@ __POS_OF__ "a";
  snap ["--req"; "a"] @@ __POS_OF__ "a";
  (**)
  let error err = Testing_cmdliner.snap_eval_error err cmd in
  error `Parse [] @@ __POS_OF__
"test_opt_req: required option \x1B[01m--req\x1B[m is \x1B[31mmissing\x1B[m
Usage: \x1B[01mtest_opt_req\x1B[m \x1B[01m--req\x1B[m=\x1B[04mARG\x1B[m [\x1B[04mOPTION\x1B[m]…
Try \x1B[01mtest_opt_req --help\x1B[m for more information.
";
    error `Term ["a"] @@ __POS_OF__
"test_opt_req: \x1B[31mtoo many arguments\x1B[m, don't know what to do with \x1B[01ma\x1B[m
Usage: \x1B[01mtest_opt_req\x1B[m \x1B[01m--req\x1B[m=\x1B[04mARG\x1B[m [\x1B[04mOPTION\x1B[m]…
Try \x1B[01mtest_opt_req --help\x1B[m for more information.
";
    error `Term ["-ra"; "a"] @@ __POS_OF__
"test_opt_req: \x1B[31mtoo many arguments\x1B[m, don't know what to do with \x1B[01ma\x1B[m
Usage: \x1B[01mtest_opt_req\x1B[m \x1B[01m--req\x1B[m=\x1B[04mARG\x1B[m [\x1B[04mOPTION\x1B[m]…
Try \x1B[01mtest_opt_req --help\x1B[m for more information.
";
    error `Parse ["-r"] @@ __POS_OF__
"test_opt_req: option \x1B[01m-r\x1B[m \x1B[31mneeds an argument\x1B[m
Usage: \x1B[01mtest_opt_req\x1B[m \x1B[01m--req\x1B[m=\x1B[04mARG\x1B[m [\x1B[04mOPTION\x1B[m]…
Try \x1B[01mtest_opt_req --help\x1B[m for more information.
";
  (**)
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
{|NAME
       test_opt_req - Test optional required arguments (don't do this)

SYNOPSIS
       test_opt_req --req=ARG [OPTION]…

OPTIONS
       -r ARG, --req=ARG (required)

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

EXIT STATUS
       test_opt_req exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.
|};
  ()

let test_arg_info_docv =
  Test.test "Arg.info default's docv on strings" @@ fun () ->
  let cmd =
    Cmd.make (Cmd.info "test_arg_docv" ~doc:"Test pos all") @@
    let+ all = Arg.(value & pos_all string [] & info [])
    and+ opt = Arg.(value & opt string "bla" & info ["field"]) in
    all, opt
  in
  let error err = Testing_cmdliner.snap_eval_error err cmd in
  (**)
  error `Term ["-z"; "a"] @@ __POS_OF__
"test_arg_docv: \x1B[31munknown\x1B[m option \x1B[01m-z\x1B[m
Usage: \x1B[01mtest_arg_docv\x1B[m [\x1B[01m--field\x1B[m=\x1B[04mVAL\x1B[m] [\x1B[04mOPTION\x1B[m]… [\x1B[04mARG\x1B[m]…
Try \x1B[01mtest_arg_docv --help\x1B[m for more information.
";
  (**)
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
{|NAME
       test_arg_docv - Test pos all

SYNOPSIS
       test_arg_docv [--field=VAL] [OPTION]… [ARG]…

OPTIONS
       --field=VAL (absent=bla)

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

EXIT STATUS
       test_arg_docv exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.
|};
  ()

let test_conv_docv =
  Test.test "Arg.Conv.docv" @@ fun () ->
  let cmd =
    let field = Arg.Conv.of_conv Arg.string ~docv:"FIELD" () in
    Cmd.make (Cmd.info "test_conv_docv" ~doc:"Test conv docv") @@
    let+ all = Arg.(value & pos_all field [] & info [])
    and+ opt = Arg.(value & opt field "bla" & info ["field"]) in
    all, opt
  in
  let error err = Testing_cmdliner.snap_eval_error err cmd in
  (**)
  error `Term ["-z"; "a"] @@ __POS_OF__
"test_conv_docv: \x1B[31munknown\x1B[m option \x1B[01m-z\x1B[m
Usage: \x1B[01mtest_conv_docv\x1B[m [\x1B[01m--field\x1B[m=\x1B[04mFIELD\x1B[m] [\x1B[04mOPTION\x1B[m]… [\x1B[04mFIELD\x1B[m]…
Try \x1B[01mtest_conv_docv --help\x1B[m for more information.
";
  (**)
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
{|NAME
       test_conv_docv - Test conv docv

SYNOPSIS
       test_conv_docv [--field=FIELD] [OPTION]… [FIELD]…

OPTIONS
       --field=FIELD (absent=bla)

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

EXIT STATUS
       test_conv_docv exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.
|};
  ()

let test_arg_file =
  Test.test "Arg.file" @@ fun () ->
  let cmd =
    Cmd.make (Cmd.info "test_arg_file" ~doc:"Test conv docv") @@
    let+ all = Arg.(value & pos_all file [] & info []) in
    all
  in
  let error err = Testing_cmdliner.snap_eval_error err cmd in
  let parse = Testing_cmdliner.snap_parse Test.T.(list string) cmd in
  parse ["-"] @@ __POS_OF__ ["-"];
  (**)
  error `Term ["-z"; "a"] @@ __POS_OF__
"test_arg_file: \x1B[31munknown\x1B[m option \x1B[01m-z\x1B[m
Usage: \x1B[01mtest_arg_file\x1B[m [\x1B[04mOPTION\x1B[m]… [\x1B[04mPATH\x1B[m]…
Try \x1B[01mtest_arg_file --help\x1B[m for more information.
";
  (**)
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
{|NAME
       test_arg_file - Test conv docv

SYNOPSIS
       test_arg_file [OPTION]… [PATH]…

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

EXIT STATUS
       test_arg_file exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.
|};
  ()

let main () =
  let doc = "Test argument specifications" in
  Test.main ~doc @@ fun () -> Test.autorun ()

let () = if !Sys.interactive then () else exit (main ())
