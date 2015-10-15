#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  let builder = `OCamlbuild_no_ocamlfind [] in
  Pkg.describe "cmdliner" ~builder [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/cmdliner";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "test/chorus.ml";
    Pkg.doc "test/cp_ex.ml";
    Pkg.doc "test/darcs_ex.ml";
    Pkg.doc "test/revolt.ml";
    Pkg.doc "test/rm_ex.ml";
    Pkg.doc "test/tail_ex.ml"; ]
