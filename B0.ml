open B0_kit.V000

(* OCaml library names *)

let cmdliner = B0_ocaml.libname "cmdliner"

(* Units *)

let cmdliner_lib = B0_ocaml.lib cmdliner ~srcs:[`Dir ~/"src"]

(* Tests *)

let test ?(requires = []) =
  B0_ocaml.test ~requires:(cmdliner :: requires)

let chorus = test ~/"test/chorus.ml"
let cp_ex = test ~/"test/cp_ex.ml"
let darcs_ex = test ~/"test/darcs_ex.ml"
let revolt = test ~/"test/revolt.ml"
let rm_ex = test ~/"test/rm_ex.ml"
let tail_ex = test ~/"test/tail_ex.ml"
let tail_ex = test ~/"test/test_nest.ml"
let test_dupe_stdopts = test ~/"test/test_dupe_stdopts.ml"
let test_man = test ~/"test/test_man.ml"
let test_man_utf8 = test ~/"test/test_man_utf8.ml"
let test_opt_req = test ~/"test/test_opt_req.ml"
let test_pos = test ~/"test/test_pos.ml"
let test_pos_all = test ~/"test/test_pos_all.ml"
let test_pos_left = test ~/"test/test_pos_left.ml"
let test_pos_req = test ~/"test/test_pos_req.ml"
let test_pos_rev = test ~/"test/test_pos_rev.ml"
let test_term_dups = test ~/"test/test_term_dups.ml"
let test_with_used_args = test ~/"test/test_with_used_args.ml"

let blueprint_min = test ~/"test/blueprint_min.ml"
let blueprint_tool = test ~/"test/blueprint_tool.ml"
let blueprint_cmds = test ~/"test/blueprint_cmds.ml"

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The cmdliner programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/cmdliner"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/cmdliner/doc"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/cmdliner/issues"
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/cmdliner.git"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.description_tags
      ["cli"; "system"; "declarative"; "org:erratique"]
    |> ~~ B0_opam.depends [ "ocaml", {|>= "4.08.0"|}; ]
    |> ~~ B0_opam.build {|[[ make "all" "PREFIX=%{prefix}%" ]]|}
    |> ~~ B0_opam.install
      {|[[make "install" "LIBDIR=%{_:lib}%" "DOCDIR=%{_:doc}%"]
         [make "install-doc" "LIBDIR=%{_:lib}%" "DOCDIR=%{_:doc}%"]]|}
    |> B0_meta.tag B0_opam.tag
  in
  B0_pack.make "default" ~doc:"cmdliner package" ~meta ~locked:true @@
  B0_unit.list ()
