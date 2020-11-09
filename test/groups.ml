open Cmdliner

let things =
  let thing =
    let doc = "thing to operate on" in
    let nfo = Arg.info ~doc [] in
     Arg.(required & pos 0 (some string) None & nfo ) in
  let show =
    let show thing =
      Printf.printf "showing %s\n" thing in
    Term.(const show $ thing)
  in
  let list =
    let list () = print_endline "listing things" in
    Term.(const list $ (const ()))
  in
  let term : _ Term.Group.t list =
    [ Term show, Term.info "show"
    ; Term list, Term.info "list"
    ]
  in
  (Term.Group.Group term), Term.info "things"

let default_cmd =
  let term =
    let run () =
      print_endline "default cmd";
        `Ok ()
    in
    Term.(ret (const run $ (const ())))
  in
  term, Term.info "groups" ~version:"%%VERSION%%"

let cmds :  _ Term.Group.t list =
  let _term (t, info) = Term.Group.Term t, info in
  [ things
  ]

let () =
  Term.(exit @@ Term.Group.eval default_cmd cmds)
