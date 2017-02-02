open Cmdliner

let pos l t r =
  print_endline (String.concat "\n" (l @ [t] @ r))

let test_pos_all =
  let l = Arg.(value & pos_left 2 string [] & info [] ~docv:"LEFT") in
  let t = Arg.(value & pos 2 string "unspecified" & info [] ~docv:"TWO") in
  let r = Arg.(value & pos_right 3 string [] & info [] ~docv:"RIGHT") in
  Term.(const pos $ l $ t $ r),
  Term.info "test_pos" ~doc:"Test pos arguments"

let () = match Term.eval test_pos_all with
| `Error _ -> exit 1
| _ -> exit 0
