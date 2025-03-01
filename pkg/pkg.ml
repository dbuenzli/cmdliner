#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

(* This is only here for `topkg distrib`. Remove once
   we switch to `b0 -- .release` *)

let () =
  let opams = [Pkg.opam_file "cmdliner.opam"] in
  Pkg.describe "cmdliner" ~opams @@ fun c ->
  Ok [ Pkg.mllib ~api:["Cmdliner"] "src/cmdliner.mllib" ]
