(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Cmdliner

(* Snapshotting command line evaluations *)

let capture_fmt f =
  let buf = Buffer.create 255 in
  let fmt = Format.formatter_of_buffer buf in
  let ret = f fmt in
  ret, (Buffer.contents buf)

let make_argv cmd args = Array.of_list (Cmd.name cmd :: args)
let env_dumb_term = function
| "TERM" -> Some "dumb"
| var -> Sys.getenv_opt var

let t_eval_result ok =
  let test_eval_error : Cmd.eval_error Test.T.t =
    let pp ppf = function
    | `Parse -> Fmt.string ppf "`Parse"
    | `Term -> Fmt.string ppf "`Term"
    | `Exn -> Fmt.string ppf "`Exn"
    in
    Test.T.make ~equal:(=) ~pp ()
  in
  let test_eval_ok ok =
    let pp ppf = function
    | `Ok v -> Test.T.pp ok ppf v
    | `Version -> Fmt.string ppf "`Version"
    | `Help -> Fmt.string ppf "`Help"
    in
    let equal v0 v1 = match v0, v1 with
    | `Ok v0, `Ok v1 -> Test.T.equal ok v0 v1
    | v0, v1 -> v0 = v1
    in
    Test.T.make ~equal ~pp ()
  in
  Test.T.result' ~ok:(test_eval_ok ok) ~error:test_eval_error

let get_eval_value ?__POS__ = function
| Ok (`Ok v) -> v
| (Error _ | Ok `Version | Ok `Help) as v ->
    Test.failstop ?__POS__ "Unexpected evalution: %a"
      (Test.T.pp (t_eval_result Test.T.any)) v

let test_eval_result ?__POS__ ?env t cmd args exp =
  let argv = make_argv cmd args in
  let (ret, _), _ = (* Ignore outputs *)
    capture_fmt @@ fun err ->
    capture_fmt @@ fun help ->
    Cmd.eval_value ?env ~help ~err cmd ~argv
  in
  Test.eq ?__POS__ (t_eval_result t) ret exp

let snap_parse ?env t cmd args exp =
  let loc = Test.Snapshot.loc exp in
  let argv = make_argv cmd args in
  let ret = Cmd.eval_value ?env cmd ~argv in
  Test.snap t (get_eval_value ~__POS__:loc ret) exp

let snap_parse_warnings ?env cmd args exp =
  let loc = Test.Snapshot.loc exp in
  let argv = make_argv cmd args in
  let ret, err = capture_fmt @@ fun err -> Cmd.eval_value ?env ~err cmd ~argv in
  ignore (get_eval_value ~__POS__:loc ret);
  Snap.lines err exp

let snap_eval_error ?env error cmd args exp =
  let loc = Test.Snapshot.loc exp in
  let argv = make_argv cmd args in
  let ret, err = capture_fmt @@ fun err -> Cmd.eval_value ?env ~err cmd ~argv in
  Test.eq (t_eval_result Test.T.any) ret (Error error) ~__POS__:loc ;
  Snap.lines err exp

let snap_help ?env retv cmd args exp =
  let loc = Test.Snapshot.loc exp in
  let argv = make_argv cmd args in
  let ret, help =
    capture_fmt @@ fun help -> Cmd.eval_value ?env ~help cmd ~argv
  in
  Test.eq (t_eval_result Test.T.any) ret retv ~__POS__:loc;
  Snap.lines help exp

let snap_completion ?env cmd args exp = snap_help ?env (Ok `Help) cmd args exp
let snap_man ?env ?(args = ["--help=plain"]) cmd exp =
  snap_help ?env (Ok `Help) cmd args exp

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
  let can_fly =
    let doc = "$(docv) indicates if the entity can fly." in
    Arg.(value & opt bool false & info ["can-fly"] ~doc)
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
    Cmd.group ~default:Term.(const (fun _ _-> ()) $ kind $ can_fly) info @@
    [fly; land']
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
