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
  Pkg.describe "cmdliner" ~distrib @@ fun c ->
  Ok [ Pkg.mllib ~api:["Cmdliner"] "src/cmdliner.mllib" ]
