(* Example from the documentation, this code is in public domain. *)

(* Implementation of the command *)

let random max min_bound max_bound count =
  let nb = ref 0 in
  Random.init 0;
  for i = 1 to count do
    let coin = Random.int max in
    if min_bound <= coin && coin <= max_bound then incr nb
  done;
  Printf.printf "%i/%i\n%!" !nb count

let check_bound max arg =
  if arg < 0
  then
    let msg = Printf.sprintf "The bounds must be greater or equal to 0,\
                              given %i" arg in
    `Error (false,msg)
  else if max < arg then
    let msg = Printf.sprintf "The bounds must be smaller or equal to the \
                              biggest value possible, here max=%i and \
                              given %i" max arg in
    `Error (false,msg)
  else
    `Ok arg

(* Command line interface *)

open Cmdliner;;

let count =
  let doc = "Throw the coin $(docv) times." in
  Arg.(value & opt int 100 & info ["c"; "count"] ~docv:"COUNT" ~doc)

let max =
  let doc = "The biggest value possible." in
  Arg.(value & opt int 9 & info ["max-rnd"; "r"] ~docv:"MAX_RND" ~doc)

let min_bound =
  let doc = "The minimal bound of counted value (must be comprised between 0\
             and the biggest value possible)." in
  let arg =
    Arg.(value & opt int 3 & info ["min-bound"; "m"] ~docv:"MAX_RND" ~doc) in
  Term.(ret (pure check_bound $ max $ arg))

let max_bound =
  let doc = "The maximum bound of counted value (must be comprised between 0\
             and the biggest value possible)." in
  let arg =
    Arg.(value & opt int 8 & info ["max-bound"; "M"] ~docv:"MAX_RND" ~doc) in
  Term.(ret (pure check_bound $ max $ arg))

let random_t = Term.(pure random $ max $ min_bound $ max_bound $ count)

let info =
  let doc = "compute the number of randomly taken value than falls inside \
             given bounds" in
  let man = [ `S "BUGS"; `P "Email bug reports to <hehey at example.org>.";] in
  Term.info "random" ~version:"1.6.1" ~doc ~man

let () = match Term.eval (random_t, info)  with `Error _ -> exit 1 | _ -> exit 0



