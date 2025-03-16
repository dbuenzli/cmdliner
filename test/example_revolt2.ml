(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

let revolt () = print_endline "Revolt!"

open Cmdliner
open Cmdliner.Term.Syntax

let cmd_revolt =
  Cmd.make (Cmd.info "revolt") @@
  let+ () = Term.const () in
  revolt ()

let main () = Cmd.eval cmd_revolt
let () = if !Sys.interactive then () else exit (main ())
