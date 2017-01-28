(* Example from the documentation, this code is in public domain. *)

let revolt () = print_endline "Revolt!"

open Cmdliner

let revolt_t = Term.(const revolt $ const ())

let () = match Term.eval (revolt_t, Term.info "revolt") with
| `Error _ -> exit 1 | _ -> exit 0
