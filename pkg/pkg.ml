#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let test t = Pkg.flatten [ Pkg.test ~run:false t; Pkg.doc (t ^ ".ml")]
let () =
  Pkg.describe "cmdliner" @@ fun c ->
  Ok [ Pkg.mllib "src/cmdliner.mllib";
       Pkg.test ~run:false "test/man_test";
       test "test/chorus";
       test "test/cp_ex";
       test "test/darcs_ex";
       test "test/revolt";
       test "test/rm_ex";
       test "test/tail_ex"; ]
