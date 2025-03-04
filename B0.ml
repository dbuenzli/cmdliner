[@@@B0.include "test/b0/B0.ml"]
(* See DEVEL.md for an explanation for the above line *)

open B0_kit.V000

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let cmdliner = B0_ocaml.libname "cmdliner"

(* Units *)

let cmdliner_lib = B0_ocaml.lib cmdliner ~srcs:[`Dir ~/"src"]

(* Tests *)

let test ?(requires = []) = B0_ocaml.test ~requires:(cmdliner :: requires)

let blueprint_min = test ~/"test/blueprint_min.ml" ~run:false
let blueprint_tool = test ~/"test/blueprint_tool.ml" ~run:false
let blueprint_cmds = test ~/"test/blueprint_cmds.ml" ~run:false

let chorus = test ~/"test/chorus.ml" ~run:false
let cp_ex = test ~/"test/cp_ex.ml" ~run:false
let darcs_ex = test ~/"test/darcs_ex.ml" ~run:false
let revolt = test ~/"test/revolt.ml" ~run:false
let rm_ex = test ~/"test/rm_ex.ml" ~run:false
let tail_ex = test ~/"test/tail_ex.ml" ~run:false

let testing = `File ~/"test/testing_cmdliner.ml"
let test_arg = test ~/"test/test_arg.ml" ~requires:[b0_std] ~srcs:[testing]
let test_term = test ~/"test/test_term.ml" ~requires:[b0_std] ~srcs:[testing]
let test_cmd = test ~/"test/test_cmd.ml" ~requires:[b0_std] ~srcs:[testing]
let test_man = test ~/"test/test_man.ml" ~requires:[b0_std] ~srcs:[testing]

(* Packs *)

(* FIXME b0 it's unclear whether the fact that the @@@B0.included units
   show up in B0_unit.list () is a bug or a feature. If it's a bug
   the filter on B0_unit.in_current_scope could be avoided. *)

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
  let locked = false (* So that it looks up b0.std *) in
  B0_pack.make "default" ~doc:"cmdliner package" ~meta ~locked @@
  List.filter B0_unit.in_current_scope (B0_unit.list ())
