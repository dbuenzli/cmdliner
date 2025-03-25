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

let test_groups =
  Test.test "Cmd.group" @@ fun () ->
  let cmd = Testing_cmdliner.sample_group_cmd in
  let parse = Testing_cmdliner.snap_parse Test.T.unit cmd in
  let error err = Testing_cmdliner.snap_eval_error err cmd in
  let warning = Testing_cmdliner.snap_parse_warnings cmd in
  parse ["birds"] @@ __POS_OF__ ();
  parse ["birds"] @@ __POS_OF__ ();
  parse ["birds"; "fly"] @@ __POS_OF__ ();
  parse ["birds"; "land"] @@ __POS_OF__ ();
  parse ["mammals"] @@ __POS_OF__ ();
  (**)
  warning ["camels"; "--bactrian"] @@ __POS_OF__
{|test_group: command 'camels': deprecated, use 'mammals' instead.
            option '--bactrian': deprecated, use nothing instead.
|};
  (**)
  error `Term [] @@ __POS_OF__
  {|test_group: required COMMAND name is missing, must be one of 'birds', 'camels', 'fishs' or 'mammals'.
Usage: test_group COMMAND …
Try 'test_group --help' for more information.
|};
  error `Term ["bla"] @@ __POS_OF__
{|test_group: unknown command 'bla', must be one of 'birds', 'camels', 'fishs' or 'mammals'.
Usage: test_group COMMAND …
Try 'test_group --help' for more information.
|};
  error `Parse ["birds"; "-k"] @@ __POS_OF__
{|test_group: option '-k' needs an argument
Usage: test_group birds [COMMAND] …
Try 'test_group birds --help' or 'test_group --help' for more information.
|};
  error `Term ["mammals"; "land"] @@ __POS_OF__
{|test_group: too many arguments, don't know what to do with 'land'
Usage: test_group mammals [OPTION]…
Try 'test_group mammals --help' or 'test_group --help' for more information.
|};
  (**)
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
{|NAME
       test_group

SYNOPSIS
       test_group COMMAND …

       Invoke command with test_group.

COMMANDS
       birds [COMMAND] …
           Operate on birds.

       fishs [OPTION]… [NAME]
           Operate on fishs.

       mammals [OPTION]…
           Operate on mammals.

       (Deprecated) camels [--bactrian] [OPTION]… [HERD]
           Operate on camels.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       test_group exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.
|};
  Testing_cmdliner.snap_man ~args:["birds"; "--help=plain"] cmd @@ __POS_OF__
  {|NAME
       test_group-birds - Operate on birds.

SYNOPSIS
       test_group birds [COMMAND] …

       Invoke command with test_group birds.

COMMANDS
       fly [--speed=SPEED] [OPTION]… [BIRD]
           Fly birds.

       land [OPTION]… [BIRD]
           Land birds.

OPTIONS
       -k VAL, --kind=VAL
           Kind of entity

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       test_group birds exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

SEE ALSO|};
  ();
  Testing_cmdliner.snap_man ~args:["birds"; "fly"; "--help=plain"] cmd @@
  __POS_OF__
 {|NAME
       test_group-birds-fly - Fly birds.

SYNOPSIS
       test_group birds fly [--speed=SPEED] [OPTION]… [BIRD]

       Invoke command with test_group birds fly.

ARGUMENTS
       BIRD (absent=pigeon)
           Use BIRD specie.

OPTIONS
       --speed=SPEED (absent=2)
           Movement SPEED in m/s

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       test_group birds fly exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

SEE ALSO|};
  Testing_cmdliner.snap_man ~args:["birds"; "land"; "--help=plain"] cmd @@
  __POS_OF__
{|NAME
       test_group-birds-land - Land birds.

SYNOPSIS
       test_group birds land [OPTION]… [BIRD]

       Invoke command with test_group birds land.

ARGUMENTS
       BIRD (absent=pigeon)
           Use BIRD specie.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       test_group birds land exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

SEE ALSO|};
  ();
  Testing_cmdliner.snap_man ~args:["fishs"; "--help=plain"] cmd @@
  __POS_OF__
    {|NAME
       test_group-fishs - Operate on fishs.

SYNOPSIS
       test_group fishs [OPTION]… [NAME]

       Invoke command with test_group fishs.

ARGUMENTS
       NAME
           Use fish named NAME.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       test_group fishs exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

SEE ALSO|};
  Testing_cmdliner.snap_man ~args:["mammals"; "--help=plain"] cmd @@
  __POS_OF__
    {|NAME
       test_group-mammals - Operate on mammals.

SYNOPSIS
       test_group mammals [OPTION]…

       Invoke command with test_group mammals.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       test_group mammals exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

SEE ALSO|};
  Testing_cmdliner.snap_man ~args:["camels"; "--help=plain"] cmd @@
  __POS_OF__
    {|NAME
       (Deprecated) test_group-camels - Operate on camels.

SYNOPSIS
       (Deprecated) test_group camels [--bactrian] [OPTION]… [HERD]

       Invoke command with test_group camels.

ARGUMENTS
       (Deprecated) HERD
           Find in herd HERD.

OPTIONS
       (Deprecated) -b, --bactrian (absent BACTRIAN env)
           Specify a bactrian camel.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       test_group camels exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

ENVIRONMENT
       These environment variables affect the execution of test_group camels:

       (Deprecated) BACTRIAN
           See option --bactrian.

SEE ALSO|};
  ()

let main () =
  let doc = "Test term specifications" in
  Test.main ~doc @@ fun () -> Test.autorun ()

let () = if !Sys.interactive then () else exit (main ())
