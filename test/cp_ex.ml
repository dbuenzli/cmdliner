(* Example from the documentation, this code is in public domain. *)

(* Implementation, we check the dest argument and print the args *)

let cp verbose recurse force srcs dest =
  if List.length srcs > 1 && 
    (not (Sys.file_exists dest) || not (Sys.is_directory dest)) 
  then 
    `Error (false, dest ^ " is not a directory") 
  else 
    `Ok (Printf.printf 
	   "verbose = %b\nrecurse = %b\nforce = %b\nsrcs = %s\ndest = %s\n" 
	    verbose recurse force (String.concat ", " srcs) dest)

(* Command line interface *)

open Cmdliner;;

let doc = "Print file names as they are copied." 
let verbose = Arg.(value & flag & info ["v"; "verbose"] ~doc) 

let doc = "Copy directories recursively."
let recurse = Arg.(value & flag & info ["r"; "R"; "recursive"] ~doc)

let doc = "If a destination file cannot be opened, remove it and try again."
let force = Arg.(value & flag & info ["f"; "force"] ~doc) 

let doc = "Source file(s) to copy."
let srcs = Arg.(non_empty & pos_left ~rev:true 0 file [] & info [] 
		~docv:"SOURCE" ~doc) 

let doc = "Destination of the copy. Must be a directory if there is more 
           than one $(i,SOURCE)."
let dest = Arg.(required & pos ~rev:true 0 (some string) None & info [] 
		~docv:"DEST" ~doc)

let cp_t = Term.(ret (pure cp $ verbose $ recurse $ force $ srcs $ dest))
let info = Term.info "cp" ~version:"1.6.1" ~doc:"copy files" ~man:
    [`S "BUGS"; `P "Email them to <hehey at example.org>.";
     `S "SEE ALSO"; `P "mv(1), scp(1), umask(2), symlink(7)"]

let () = match Term.eval info cp_t with `Error _ -> exit 1 | _ -> exit 0
