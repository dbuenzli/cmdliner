open Cmdliner

let getenv n =
  try Sys.getenv n with Not_found -> ""

let main () =
  let args =
    let parse = fst Arg.(list ~sep:',' (pair ~sep:':' string int)) in
    match parse (getenv "ARGS") with
    | `Ok args -> args
    | `Error _ ->
        prerr_endline
          "Error: $ARGS could not be parsed, should be \
           of the form <name1>:<int1>,<name2>:<int2>,...";
        exit 1
  in
  let args = List.map (fun (n, k) -> (k, Arg.int, Arg.info [ n ])) args in
  if List.length args = 0 then
    prerr_endline "No arguments specified.  Try setting $ARGS: `ARGS=foo:0,bar:101 ./dynargs.native`"
  else
  let cmd = Term.(pure (fun _ -> `Help (`Pager, None)) $ Arg.(value & vopt args)) in
  ignore (Term.(eval (ret cmd, info "dynargs")))

let _ =
  main ()
