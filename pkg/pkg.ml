#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let test t = Pkg.flatten [ Pkg.test ~run:false t; Pkg.doc (t ^ ".ml")]
let () =
  Pkg.describe "cmdliner" @@ fun c ->
  Ok [ Pkg.mllib ~api:["Cmdliner"] "src/cmdliner.mllib";
       test "test/chorus";
       test "test/cp_ex";
       test "test/darcs_ex";
       test "test/revolt";
       test "test/rm_ex";
       test "test/tail_ex";
       Pkg.test ~run:false "test/test_man";
       Pkg.test ~run:false "test/test_err"; ]
