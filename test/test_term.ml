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

let test_with_used_args =
  Test.test "Term.with_used_args" @@ fun () ->
  let cmd =
    Cmd.make (Cmd.info "test_with_used_args" ~doc:"Test cli arg capture") @@
    let args =
      let+ a = Arg.(value & flag & info ["a"; "aaa"])
      and+ b = Arg.(value & opt (some string) None & info ["b"; "bbb"])
      and+ c = Arg.(value & pos_all string [] & info []) in
      (a, b, c)
    in
    let+ parse, args = Term.with_used_args args in
    args, parse
  in
  let t = Test.T.(t2 (list string) (t3 bool (option string) (list string))) in
  let parse = Testing_cmdliner.snap_parse t cmd in
  let error err = Testing_cmdliner.snap_eval_error err cmd in
  parse [] @@ __POS_OF__
    ([], (false, None, []));
  (* Note some of these are bugs, see issue #204 *)
  parse ["--"] @@ __POS_OF__
    ([], (false, None, []));
  parse ["hoho"; "-a"; "-bmsg"] @@ __POS_OF__
    (["-a"; "-b"; "msg"; "hoho"], (true, Some "msg", ["hoho"]));
  parse ["hoho"; "-a"; "-bmsg"; "hihi"] @@ __POS_OF__
    (["-a"; "-b"; "msg"; "hoho"; "hihi"], (true, Some "msg", ["hoho"; "hihi"]));
  parse ["--"; "hoho"; "-a"; "-bbla"; "hihi"] @@ __POS_OF__
    (["hoho"; "-a"; "-bbla"; "hihi"],
     (false, None, ["hoho"; "-a"; "-bbla"; "hihi"]));
  parse ["hoho"; "-a"; "--bbb=msg"; "hihi"] @@ __POS_OF__
    (["-a"; "--bbb"; "msg"; "hoho"; "hihi"],
     (true, Some "msg", ["hoho"; "hihi"]));
  (**)
  error `Term ["--opt"] @@ __POS_OF__
"Usage: \u{001B}[01mtest_with_used_args\u{001B}[m [\u{001B}[01m--help\u{001B}[m] [\u{001B}[01m--aaa\u{001B}[m] [\u{001B}[01m--bbb\u{001B}[m=\u{001B}[04mVAL\u{001B}[m] [\u{001B}[04mOPTION\u{001B}[m]… [\u{001B}[04mARG\u{001B}[m]…\n\
test_with_used_args: \u{001B}[31munknown\u{001B}[m option \u{001B}[01m--opt\u{001B}[m\n";
  (**)
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
{|NAME
       test_with_used_args - Test cli arg capture

SYNOPSIS
       test_with_used_args [--aaa] [--bbb=VAL] [OPTION]… [ARG]…

OPTIONS
       -a, --aaa

       -b VAL, --bbb=VAL

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

EXIT STATUS
       test_with_used_args exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.
|};
  ()

let term_duplication =
  Test.test "Term.app duplicates" @@ fun () ->
  let cmd =
    Cmd.make (Cmd.info "test_term_dups" ~doc:"Test multiple term usage") @@
    let+ p =
      let doc = "First pos argument should show up only once in the docs" in
      Arg.(value & pos 0 string "popopo" & info [] ~doc ~docv:"POS")
    and+ o =
      let doc = "This should show up only once in the docs" in
      Arg.(value & flag & info ["f"; "flag"] ~doc)
    in
    (p, p, o, o)
  in
  let t = Test.T.(t4 string string bool bool) in
  let parse = Testing_cmdliner.snap_parse t cmd in
  let error err = Testing_cmdliner.snap_eval_error err cmd in
  parse [] @@ __POS_OF__ ("popopo", "popopo", false, false);
  parse ["0"] @@ __POS_OF__ ("0", "0", false, false);
  parse ["0"; "-f"] @@ __POS_OF__ ("0", "0", true, true);
  (**)
  error `Term ["0"; "1"] @@ __POS_OF__
"Usage: \u{001B}[01mtest_term_dups\u{001B}[m [\u{001B}[01m--help\u{001B}[m] [\u{001B}[01m--flag\u{001B}[m] [\u{001B}[04mOPTION\u{001B}[m]… [\u{001B}[04mPOS\u{001B}[m]\n\
test_term_dups: \u{001B}[31mtoo many arguments\u{001B}[m, don't know what to do with \u{001B}[01m1\u{001B}[m\n";
  (**)
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
{|NAME
       test_term_dups - Test multiple term usage

SYNOPSIS
       test_term_dups [--flag] [OPTION]… [POS]

ARGUMENTS
       POS (absent=popopo)
           First pos argument should show up only once in the docs

OPTIONS
       -f, --flag
           This should show up only once in the docs

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

EXIT STATUS
       test_term_dups exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.
|};
  ()

let test_env =
  Test.test "Term.env" @@ fun () ->
  let env = function "HEYHO" -> Some "Let's go" | _ -> None in
  let cmd =
    Cmd.make (Cmd.info "test_env" ~doc:"Test Term.env") @@
    let+ env = Term.env in
    Test.(option T.string) (env "HEYHO") (Some "Let's go")
  in
  Testing_cmdliner.test_eval_result ~env Test.T.unit cmd [] (Ok (`Ok ()));
  ()


let main () =
  let doc = "Test term specifications" in
  Test.main ~doc @@ fun () -> Test.autorun ()

let () = if !Sys.interactive then () else exit (main ())
