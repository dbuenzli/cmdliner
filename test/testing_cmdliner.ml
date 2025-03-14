(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Cmdliner

(* Snapshotting command line evaluations *)

let get_eval_value ?__POS__ = function
| Ok (`Ok v) -> v
| Error _ | Ok `Version | Ok `Help ->
    Test.failstop ?__POS__ "Unexpected evaluation"

let capture_fmt f =
  let buf = Buffer.create 255 in
  let fmt = Format.formatter_of_buffer buf in
  let ret = f fmt in
  ret, (Buffer.contents buf)

let make_argv cmd args = Array.of_list (Cmd.name cmd :: args)

let snap_parse t cmd args exp =
  let loc = Test.Snapshot.loc exp in
  let argv = make_argv cmd args in
  let ret = Cmd.eval_value cmd ~argv in
  Test.snap t (get_eval_value ~__POS__:loc ret) exp

let snap_parse_warnings cmd args exp =
  let loc = Test.Snapshot.loc exp in
  let argv = make_argv cmd args in
  let ret, err = capture_fmt @@ fun err -> Cmd.eval_value ~err cmd ~argv in
  ignore (get_eval_value ~__POS__:loc ret);
  Snap.lines err exp

let snap_eval_error error cmd args exp =
  let loc = Test.Snapshot.loc exp in
  let argv = make_argv cmd args in
  let ret, err = capture_fmt @@ fun err -> Cmd.eval_value ~err cmd ~argv in
  Test.any ret (Error error) ~__POS__:loc ;
  Snap.lines err exp

let snap_man ?(args = ["--help=plain"]) cmd exp =
  let loc = Test.Snapshot.loc exp in
  let argv = make_argv cmd args in
  let ret, help = capture_fmt @@ fun help -> Cmd.eval_value ~help cmd ~argv in
  Test.any ret (Ok `Help) ~__POS__:loc;
  Snap.lines help exp
