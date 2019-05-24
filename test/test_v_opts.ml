open Cmdliner

let options =
  let short_option =
    let doc = "Print out $(docv) as a short option." in
    let docv = "ARG" in
    Arg.string, `Short, Arg.info ~doc ~docv ["s"] in
  let long_option =
    let doc = "Print out $(docv) as a long option." in
    let docv = "ARG" in
    Arg.string, `Long, Arg.info ~doc ~docv ["long"] in
  [short_option; long_option]

let show_args =
  List.iter @@ function 
  | (_, `NotPresent) ->
    print_endline "Option not present"
  | (v, `Short) ->
    Format.printf "Short: %s\n" v
  | (v, `Long) ->
    Format.printf "Long: %s\n" v

let show_arg v = 
  show_args [v]
  
let v_opt_cmd =
  let v_opt_term = 
    Arg.(value & v_opt ("", `NotPresent) options) in
  Term.(const show_arg $ v_opt_term)
let v_opt_vopt_cmd =
  let v_opt_vopt_term =
    let vopt = "Default value" in
    Arg.(value & v_opt ~vopt ("", `NotPresent) options) in
  Term.(const show_arg $ v_opt_vopt_term)
let v_opt_all_cmd =
  let v_opt_all_term =
    Arg.(value & v_opt_all [] options) in
  Term.(const show_args $ v_opt_all_term)
let v_opt_all_vopt_cmd =
  let v_opt_all_vopt_term =
    let vopt = "Default value" in
    Arg.(value & v_opt_all ~vopt [] options) in
  Term.(const show_args $ v_opt_all_vopt_term)

let cmds = [
    v_opt_cmd, Term.info ~doc:"At most one flag (no vopt)" "v_opt" ; 
    v_opt_vopt_cmd, Term.info ~doc:"At most one flag (with vopt)" "v_opt_vopt" ; 
    v_opt_all_cmd, Term.info ~doc:"All flags (no vopt)" "v_opt_all" ;
    v_opt_all_vopt_cmd, Term.info ~doc:"All flags (with vopt)" "v_opt_all_vopt" ;
  ]

let default_cmd =
  let doc = "Test program for the v_opt arguments" in
  let sdocs = Cmdliner.Manpage.s_common_options in
  let exits = Cmdliner.Term.default_exits in
  Cmdliner.Term.(ret (const (fun _ -> `Help (`Pager, None)) $ v_opt_cmd)),
  Cmdliner.Term.info "test_v_opt_all" ~doc ~sdocs ~exits

let () = Cmdliner.Term.(exit @@ eval_choice default_cmd cmds)

(* Test Output

> ./test_v_opts.exe v_opt
Option not present
> ./test_v_opts.exe v_opt -s hello
Short: hello
> ./test_v_opts.exe v_opt -s hello --long world
test_v_opt_all: options `-s' and `--long' cannot be present at the same time
Usage: test_v_opt_all v_opt [OPTION]... 
Try `test_v_opt_all v_opt --help' or `test_v_opt_all --help' for more information.

> ./test_v_opts.exe v_opt_vopt
Option not present
> ./test_v_opts.exe v_opt_vopt -s hello
Short: hello
> ./test_v_opts.exe v_opt_vopt -s
Short: Default value
> ./test_v_opts.exe v_opt_vopt -s hello --long world
test_v_opt_all: options `-s' and `--long' cannot be present at the same time
Usage: test_v_opt_all v_opt_vopt [OPTION]... 
Try `test_v_opt_all v_opt_vopt --help' or `test_v_opt_all --help' for more information.
> ./test_v_opts.exe v_opt_vopt -s hello --long
test_v_opt_all: options `-s' and `--long' cannot be present at the same time
Usage: test_v_opt_all v_opt_vopt [OPTION]... 
Try `test_v_opt_all v_opt_vopt --help' or `test_v_opt_all --help' for more information.
> ./test_v_opts.exe v_opt_vopt --long
Long: Default value
> ./test_v_opts.exe v_opt_vopt --long world
Long: world

> ./test_v_opts.exe v_opt_all -s hello --long world
Short: hello
Long: world
> ./test_v_opts.exe v_opt_all -s hello --long
test_v_opt_all: option `--long' needs an argument
Usage: test_v_opt_all v_opt_all [OPTION]... 
Try `test_v_opt_all v_opt_all --help' or `test_v_opt_all --help' for more information.

> ./test_v_opts.exe v_opt_all_vopt -s hello --long world
Short: hello
Long: world
> ./test_v_opts.exe v_opt_all_vopt -s hello --long
Short: hello
Long: Default value
> ./test_v_opts.exe v_opt_all_vopt -s --long
Short: Default value
Long: Default value
> ./test_v_opts.exe v_opt_all_vopt -s --long -s
Short: Default value
Long: Default value
Short: Default value

*)