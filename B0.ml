open B0_kit.V000
open B00_std

(* Units *)

let cmdliner = B0_ocaml.libname "cmdliner"
let cmdliner_lib =
  B0_ocaml.lib cmdliner ~srcs:Fpath.[`Dir (v "src")] ~requires:[]

(* Packs *)

let default =
  let units = [cmdliner_lib] in
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The cmdliner programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/cmdliner"
    |> add online_doc "https://erratique.ch/software/cmdliner/doc"
    |> add description_tags ["cli"; "system"; "declarative"; "org:erratique"]
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/cmdliner.git"
    |> add issues "https://github.com/dbuenzli/cmdliner/issues"
    |> tag B0_opam.tag
  in
  B0_pack.v "default" ~doc:"cmdliner" ~meta ~locked:true units
