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

let verbose = 
  let doc = "Print file names as they are copied." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc) 

let recurse = 
  let doc = "Copy directories recursively." in
  Arg.(value & flag & info ["r"; "R"; "recursive"] ~doc)

let force = 
  let doc = "If a destination file cannot be opened, remove it and try again."in
  Arg.(value & flag & info ["f"; "force"] ~doc)

let srcs = 
  let doc = "Source file(s) to copy." in
  Arg.(non_empty & pos_left ~rev:true 0 file [] & info [] ~docv:"SOURCE" ~doc) 

let dest = 
  let doc = "Destination of the copy. Must be a directory if there is more 
           than one $(i,SOURCE)." in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv:"DEST" 
         ~doc)

let cmd = 
  let doc = "copy files" in
  let man = [
    `S "BUGS"; 
    `P "Email them to <hehey at example.org>."; 
    `S "SEE ALSO";
    `P "$(b,mv)(1), $(b,scp)(1), $(b,umask)(2), $(b,symlink)(7)" ]
  in
  Term.(ret (pure cp $ verbose $ recurse $ force $ srcs $ dest)), 
  Term.info "cp" ~version:"1.6.1" ~doc ~man

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
