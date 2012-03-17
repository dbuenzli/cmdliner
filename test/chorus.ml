(* Example from the documentation, this code is in public domain. *)

(* Implementation of the command *) 

let chorus count msg = for i = 1 to count do print_endline msg done

(* Command line interface *)

open Cmdliner;;

let count = 
  let doc = "Repeat the message $(docv) times." in
  Arg.(value & opt int 10 & info ["c"; "count"] ~docv:"COUNT" ~doc)

let msg = 
  let doc = "The message to print." in
  Arg.(value & pos 0 string "Revolt!" & info [] ~docv:"MSG" ~doc)

let chorus_t = Term.(pure chorus $ count $ msg) 

let info = 
  let doc = "print a customizable message repeatedly" in
  let man = [ `S "BUGS"; `P "Email bug reports to <hehey at example.org>.";] in
  Term.info "chorus" ~version:"1.6.1" ~doc ~man
    
let () = match Term.eval (chorus_t, info)  with `Error _ -> exit 1 | _ -> exit 0



