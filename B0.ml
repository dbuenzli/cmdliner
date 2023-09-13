open B0_kit.V000

(* OCaml library names *)

let cmdliner = B0_ocaml.libname "cmdliner"

(* Units *)

let cmdliner_lib =
  let srcs = Fpath.[`Dir (v "src")] in
  B0_ocaml.lib cmdliner ~doc:"The cmdliner library" ~srcs ~requires:[]

(* Tests *)

let test ?doc t =
  let srcs = [`File (Fpath.v (Fmt.str "test/%s.ml" t))] in
  let requires = [cmdliner] in
  let meta = B0_meta.(empty |> tag test) in
  B0_ocaml.exe t ?doc ~srcs ~requires ~meta

let chorus = test "chorus"
let cp_ex = test "cp_ex"
let darcs_ex = test "darcs_ex"
let revolt = test "revolt"
let rm_ex = test "rm_ex"
let tail_ex = test "tail_ex"
let tail_ex = test "test_nest"
let test_dupe_stdopts = test "test_dupe_stdopts"
let test_man = test "test_man"
let test_man_utf8 = test "test_man_utf8"
let test_opt_req = test "test_opt_req"
let test_pos = test "test_pos"
let test_pos_all = test "test_pos_all"
let test_pos_left = test "test_pos_left"
let test_pos_req = test "test_pos_req"
let test_pos_rev = test "test_pos_rev"
let test_term_dups = test "test_term_dups"
let test_with_used_args = test "test_with_used_args"

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The cmdliner programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/cmdliner"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/cmdliner/doc"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/cmdliner/issues"
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/cmdliner.git"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add description_tags)
       ["cli"; "system"; "declarative"; "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|}; ]
    |> B0_meta.add B0_opam.build
      {|[[ make "all" "PREFIX=%{prefix}%" ]]|}
    |> B0_meta.add B0_opam.install
      {|[[make "install" "LIBDIR=%{_:lib}%" "DOCDIR=%{_:doc}%"]
         [make "install-doc" "LIBDIR=%{_:lib}%" "DOCDIR=%{_:doc}%"]]|}
  in
  B0_pack.make "default" ~doc:"cmdliner package" ~meta ~locked:true @@
  B0_unit.list ()
