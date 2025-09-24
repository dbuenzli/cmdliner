#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

(* This is only here for `topkg distrib`. Remove once
   we switch to `b0 -- .release` *)

let distrib =
  (* The default removes Makefile *)
  let exclude_paths () = Ok [".git";".gitignore";".gitattributes";"_build"] in
  Pkg.distrib ~exclude_paths ()

let () =
  let opams = [Pkg.opam_file "cmdliner.opam"] in
  Pkg.describe "cmdliner" ~distrib ~opams @@ fun c ->
  Ok [ Pkg.mllib ~api:["Cmdliner"] "src/cmdliner.mllib" ]
