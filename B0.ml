[@@@B0.include "test/b0/B0.ml"]
(* See DEVEL.md for an explanation for the above line *)

open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let cmdliner = B0_ocaml.libname "cmdliner"

(* Units *)

let cmdliner_lib =
  B0_ocaml.lib cmdliner ~name:"cmdliner-lib" ~srcs:[`Dir ~/"src"]

(* Tool *)

let cmdliner_tool =
  let srcs = [`Dir ~/"src/tool"] in
  B0_ocaml.exe "cmdliner" ~public:true ~srcs ~requires:[cmdliner]

(* Tests *)

let test ?(requires = []) = B0_ocaml.test ~requires:(cmdliner :: requires)

let testing = `File ~/"test/testing_cmdliner.ml"

let test_shell = test ~/"test/test_shell.ml" ~run:false
let test_arg = test ~/"test/test_arg.ml" ~srcs:[testing] ~requires:[b0_std]
let test_cmd = test ~/"test/test_cmd.ml" ~srcs:[testing] ~requires:[b0_std]
let test_completion =
  test ~/"test/test_completion.ml" ~srcs:[testing] ~requires:[b0_std]

let test_deprecation =
  test ~/"test/test_deprecation.ml" ~srcs:[testing] ~requires:[b0_std]

let test_legacy_prefix =
  test ~/"test/test_legacy_prefix.ml" ~srcs:[testing] ~requires:[b0_std]

let test_man = test ~/"test/test_man.ml" ~srcs:[testing] ~requires:[b0_std]
let test_term = test ~/"test/test_term.ml" ~srcs:[testing] ~requires:[b0_std]

let example_chorus = test ~/"test/example_chorus.ml" ~run:false
let example_cp = test ~/"test/example_cp.ml" ~run:false
let example_darcs = test ~/"test/example_darcs.ml" ~run:false
let example_group =
  let srcs = [testing] and requires = [b0_std] in
  test ~/"test/example_group.ml" ~run:false ~srcs ~requires

let example_revolt1 = test ~/"test/example_revolt1.ml" ~run:false
let example_revolt2 = test ~/"test/example_revolt2.ml" ~run:false
let example_rm = test ~/"test/example_rm.ml" ~run:false
let example_tail = test ~/"test/example_tail.ml" ~run:false

let blueprint_min = test ~/"test/blueprint_min.ml" ~run:false
let blueprint_tool = test ~/"test/blueprint_tool.ml" ~run:false
let blueprint_cmds = test ~/"test/blueprint_cmds.ml" ~run:false

(* Completion scripts update *)

let update_completion_scripts =
  B0_unit.of_action "update-cmdliner-data" @@ fun env _ ~args:_ ->
  let bash = B0_env.in_scope_dir env ~/"src/tool/bash-completion.sh" in
  let zsh = B0_env.in_scope_dir env ~/"src/tool/zsh-completion.sh" in
  let ml = B0_env.in_scope_dir env ~/"src/tool/cmdliner_data.ml" in
  let* bash = Os.File.read bash in
  let* zsh = Os.File.read zsh in
  let src = Fmt.str
      "let bash_generic_completion =\n{|%s\
       |}\n\n\
       let zsh_generic_completion =\n{|%s\
       |}" bash zsh
  in
  Os.File.write ~force:true ~make_path:false ml src

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
{|[[make "install" "BINDIR=%{_:bin}%" "LIBDIR=%{_:lib}%" "DOCDIR=%{_:doc}%"
         "SHAREDIR=%{share}%" "MANDIR=%{man}%"]
   [make "install-doc" "LIBDIR=%{_:lib}%" "DOCDIR=%{_:doc}%"
         "SHAREDIR=%{share}%" "MANDIR=%{man}%"]]|}
    |> B0_meta.tag B0_opam.tag
  in
  let locked = false (* So that it looks up b0.std *) in
  B0_pack.make "default" ~doc:"cmdliner package" ~meta ~locked @@
  List.filter B0_unit.in_current_scope (B0_unit.list ())
