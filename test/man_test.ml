
open Cmdliner

let man_test_t = Term.(pure ())

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
    `S "BUGS";
    `P "Email bug reports to <hehey at example.org>.";]
  in
  Term.info "man_test" ~version:"1.6.1" ~doc ~man

let () = match Term.eval (man_test_t, info) with
| `Error _ -> exit 1
| _ -> exit 0
