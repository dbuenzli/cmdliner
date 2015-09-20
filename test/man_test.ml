
open Cmdliner

let hey =
  let doc = "Equivalent to set $(opt)." in
  let env = Arg.env_var "TEST_ENV" ~doc in
  let doc = "Set hey." in
  Arg.(value & flag & info ["hey"] ~env ~doc)

let repodir =
  let doc = "See option $(opt)." in
  let env = Arg.env_var "TEST_REPODDIR" ~doc in
  let doc = "Run the program in repository directory $(docv)." in
  Arg.(value & opt file Filename.current_dir_name & info ["repodir"] ~env
         ~docv:"DIR" ~doc)

let id =
  let doc = "See option $(opt)." in
  let env = Arg.env_var "TEST_ID" ~doc in
  let doc = "Whatever $(docv) bla $(env) and $(opt)." in
  Arg.(value & opt int ~vopt:10 0 & info ["id"; "i"] ~env ~docv:"ID" ~doc)

let miaouw =
  let doc = "See option $(opt)." in
  let docs = "MIAOUW SECTION" in
  let env = Arg.env_var "TEST_MIAOUW" ~doc ~docs in
  let doc = "Whatever this is the doc var $(docv) this is the env var $(env) \
             this is the opt $(opt)."
  in
  Arg.(value & opt string "miaouw" & info ["m";] ~env ~docv:"MIAOUW" ~doc)

let test hey repodir id miaouw =
  Format.printf "hey: %b@.repodir: %s@.id: %d@.miaouw: %s@."
    hey repodir id miaouw

let man_test_t = Term.(pure test $ hey $ repodir $ id $ miaouw)

let info =
  let doc = "print a customizable message repeatedly" in
  let man = [
    `S "THIS IS A SECTION  FOR $(mname)";
    `P "This is a paragraph";
    `Pre "This is a preformatted paragraph for $(mname) no filling will \
          occur do the $(i,ASCII) art $(b,here) this will overflow on 80 \
          columns \n\
          01234556789\
          01234556789\
          01234556789\
          01234556789\
          01234556789\
          01234556789\
          01234556789\
          01234556789\n\
          +---+\n\
          |  /|\n\
          | / | ----> Let's swim to the moon.\n\
          |/  |\n\
          +---+";
    `P "This is another paragraph $(bla) $(i,$(bla)) bla";
    `Noblank;
    `Pre "This is another preformatted paragraph.\n\
          There should be no blanks before and after it.";
    `Noblank;
    `P "Hey ho";
    `S "ENVIRONMENT VARIABLES"; (* specify where env need to be *)
    `S "BUGS";
    `P "Email bug reports to <hehey at example.org>.";]
  in
  Term.info "man_test" ~version:"1.6.1" ~doc ~man

let () = match Term.eval (man_test_t, info) with
| `Error _ -> exit 1
| _ -> exit 0
