(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Implementation of the command *)

let chorus ~count msg = for i = 1 to count do print_endline msg done

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let count =
  let doc = "Repeat the message $(docv) times." in
  Arg.(value & opt int 10 & info ["c"; "count"] ~doc ~docv:"COUNT")

let msg =
  let env =
    let doc = "Overrides the default message to print." in
    Cmd.Env.info "CHORUS_MSG" ~doc
  in
  let doc = "The message to print." in
  Arg.(value & pos 0 string "Revolt!" & info [] ~env ~doc ~docv:"MSG")

let chorus_cmd =
  let doc = "Print a customizable message repeatedly" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <bugs@example.org>." ]
  in
  Cmd.make (Cmd.info "chorus" ~version:"%%VERSION%%" ~doc ~man) @@
  let+ count and+ msg in
  chorus ~count msg

let main () = Cmd.eval chorus_cmd
let () = if !Sys.interactive then () else exit (main ())
