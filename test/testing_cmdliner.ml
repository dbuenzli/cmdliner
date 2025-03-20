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

let snap_completion cmd args exp = snap_man cmd ~args exp

(* Sample commands *)

open Cmdliner
open Cmdliner.Term.Syntax

let sample_group_cmd =
  let man = [ `P "Invoke command with $(iname)." ] in
  let kind =
    let doc = "Kind of entity" in
    Arg.(value & opt (some string) None & info ["k";"kind"] ~doc)
  in
  let speed =
    let doc = "Movement $(docv) in m/s" in
    Arg.(value & opt int 2 & info ["speed"] ~doc ~docv:"SPEED")
  in
  let birds =
    let bird =
      let doc = "Use $(docv) specie." in
      Arg.(value & pos 0 string "pigeon" & info [] ~doc ~docv:"BIRD")
    in
    let fly =
      Cmd.make (Cmd.info "fly" ~doc:"Fly birds." ~man) @@
      let+ bird and+ speed in ()
    in
    let land' =
      Cmd.make (Cmd.info "land" ~doc:"Land birds." ~man) @@
      let+ bird in ()
    in
    let info = Cmd.info "birds" ~doc:"Operate on birds." ~man in
    Cmd.group ~default:Term.(const (fun k -> ()) $ kind) info [fly; land']
  in
  let mammals =
    let man_xrefs = [`Main; `Cmd "birds" ] and doc = "Operate on mammals." in
    Cmd.make (Cmd.info "mammals" ~doc ~man_xrefs ~man) @@
    Term.(const (fun () -> ()) $ const ())
  in
  let fishs =
    let name' =
      let doc = "Use fish named $(docv)." in
      Arg.(value & pos 0 (some string) None & info [] ~doc ~docv:"NAME")
    in
    Cmd.make (Cmd.info "fishs" ~doc:"Operate on fishs." ~man) @@
    let+ name' in ()
  in
  let camels =
    let herd =
      let doc = "Find in herd $(docv)." and docv = "HERD" in
      let deprecated = "deprecated, herds are ignored." in
      Arg.(value & pos 0 (some string) None & info [] ~deprecated ~doc ~docv)
    in
    let bactrian =
      let deprecated = "deprecated, use nothing instead." in
      let doc = "Specify a bactrian camel." in
      let env = Cmd.Env.info "BACTRIAN" ~deprecated in
      Arg.(value & flag & info ["bactrian"; "b"] ~deprecated ~env ~doc)
    in
    let deprecated = "deprecated, use 'mammals' instead." in
    Cmd.make (Cmd.info "camels" ~deprecated ~doc:"Operate on camels." ~man) @@
    let+ bactrian and+ herd in ()
  in
  Cmd.group (Cmd.info "test_group" ~version:"X.Y.Z" ~man) @@
  [birds; mammals; fishs; camels]
