(* Example from the documentation, this code is in public domain. *)

let revolt () = print_endline "Revolt!"

open Cmdliner

let revolt_t = Term.(const revolt $ const ())
let cmd = Cmd.v (Cmd.info "revolt") revolt_t
let main () = Cmd.eval cmd
let () = if !Sys.interactive then () else exit (main ())
