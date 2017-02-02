open Cmdliner

let err_test strict a0 a1 a2 = ()

let err_test =
  let req p docv = Arg.(required & pos p (some string) None & info [] ~docv) in
  let pkg = req 0 "PKG" in
  let version = req 1 "VERSION" in
  let platform = req 2 "PLATFORM" in
  let strict = Arg.(value & flag & info ["strict"]) in
  let doc = "Test required positional argument error reporting" in
  Term.(const err_test $ strict $ pkg $ version $ platform),
  Term.info "err_test" ~doc

let () = match Term.eval err_test with
| `Error _ -> exit 1
| _ -> exit 0
