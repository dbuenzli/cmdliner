(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

let revolt () = print_endline "Revolt!"

open Cmdliner

let revolt_term = Term.app (Term.const revolt) (Term.const ())
let revolt_cmd = Cmd.v (Cmd.info "revolt") revolt_term
let main () = Cmd.eval revolt_cmd
let () = if !Sys.interactive then () else exit (main ())
