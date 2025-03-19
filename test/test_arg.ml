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
  {|test_pos_all: unknown option '--opt'.
Usage: test_pos_all [OPTION]… [THEARG]…
Try 'test_pos_all --help' for more information.
|};
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
  {|test_pos_left: too many arguments, don't know what to do with '2'
Usage: test_pos_left [OPTION]… [LEFT] [LEFT]
Try 'test_pos_left --help' for more information.
|};
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
{|test_pos_req: required arguments R1, R2, R3, RIGHT are missing
Usage: test_pos_req [OPTION]… R1 R2 R3 RIGHT…
Try 'test_pos_req --help' for more information.
|};
  error ["r1"] @@ __POS_OF__
{|test_pos_req: required arguments R2, R3, RIGHT are missing
Usage: test_pos_req [OPTION]… R1 R2 R3 RIGHT…
Try 'test_pos_req --help' for more information.
|};
  error ["r1"; "r2"] @@ __POS_OF__
{|test_pos_req: required arguments R3, RIGHT are missing
Usage: test_pos_req [OPTION]… R1 R2 R3 RIGHT…
Try 'test_pos_req --help' for more information.
|};
  error ["r1"; "r2"; "r3"] @@ __POS_OF__
{|test_pos_req: required argument RIGHT is missing
Usage: test_pos_req [OPTION]… R1 R2 R3 RIGHT…
Try 'test_pos_req --help' for more information.
|};
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
{|test_opt_req: required option --req is missing
Usage: test_opt_req --req=ARG [OPTION]…
Try 'test_opt_req --help' for more information.
|};
    error `Term ["a"] @@ __POS_OF__
{|test_opt_req: too many arguments, don't know what to do with 'a'
Usage: test_opt_req --req=ARG [OPTION]…
Try 'test_opt_req --help' for more information.
|};
    error `Term ["-ra"; "a"] @@ __POS_OF__
{|test_opt_req: too many arguments, don't know what to do with 'a'
Usage: test_opt_req --req=ARG [OPTION]…
Try 'test_opt_req --help' for more information.
|};
    error `Parse ["-r"] @@ __POS_OF__
{|test_opt_req: option '-r' needs an argument
Usage: test_opt_req --req=ARG [OPTION]…
Try 'test_opt_req --help' for more information.
|};
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

let test_conv_docv =
  Test.test "Arg.Conv docv" @@ fun () ->
  let cmd =
    let field = Arg.Conv.of_conv Arg.string ~docv:"FIELD" () in
    Cmd.make (Cmd.info "test_pos_all" ~doc:"Test pos all") @@
    let+ all = Arg.(value & pos_all field [] & info [])
    and+ opt = Arg.(value & opt field "bla" & info ["field"]) in
    all, opt
  in
  let error err = Testing_cmdliner.snap_eval_error err cmd in
  (**)
  error `Term ["-z"; "a"] @@ __POS_OF__
{|test_pos_all: unknown option '-z'.
Usage: test_pos_all [--field=VAL] [OPTION]… [ARG]…
Try 'test_pos_all --help' for more information.
|};
  (**)
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
{|NAME
       test_pos_all - Test pos all

SYNOPSIS
       test_pos_all [--field=VAL] [OPTION]… [ARG]…

OPTIONS
       --field=VAL (absent=bla)

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

let main () =
  let doc = "Test argument specifications" in
  Test.main ~doc @@ fun () -> Test.autorun ()

let () = if !Sys.interactive then () else exit (main ())
