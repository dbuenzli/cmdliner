(* Example from the documentation, this code is in public domain. *)

(* Implementations, just print the args. *)

type verb = Normal | Quiet | Verbose
type copts = { debug : bool; verb : verb; prehook : string option }

let str = Printf.sprintf 
let opt_str sv = function None -> "None" | Some v -> str "Some(%s)" (sv v)
let opt_str_str = opt_str (fun s -> s)
let verb_str = function 
  | Normal -> "normal" | Quiet -> "quiet" | Verbose -> "verbose"

let pr_copts oc copts = Printf.fprintf oc 
    "debug = %b\nverbosity = %s\nprehook = %s\n" 
    copts.debug (verb_str copts.verb) (opt_str_str copts.prehook)

let initialize copts repodir = Printf.printf
    "%arepodir = %s\n" pr_copts copts repodir

let record copts name email all ask_deps files = Printf.printf
    "%aname = %s\nemail = %s\nall = %b\nask-deps = %b\nfiles = %s\n" 
    pr_copts copts (opt_str_str name) (opt_str_str email) all ask_deps 
    (String.concat ", " files)

let help copts cmds topic = match topic with
| None -> `Help (`Pager, None) (* help about the program. *)
| Some topic -> 
    if List.mem topic cmds then `Help (`Pager, (Some topic)) else
    let page = (topic, 7, "", "", ""), [`S topic; `P "Say something";] in
    `Ok (Cmdliner.Manpage.print `Pager Format.std_formatter page)

open Cmdliner;;

(* Help sections common to all commands *)

let copts_sect = "COMMON OPTIONS"
let help_secs = [ 
  `S copts_sect; 
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use `darcs $(i,COMMAND) --help' for help on a single command.";`Noblank;
  `P "Use `darcs help patterns' for help on patch matching."; `Noblank;
  `P "Use `darcs help environment' for help on environment variables.";
  `S "BUGS"; `P "Check bug reports at http://bugs.example.org.";]

(* Options common to all commands *)

let copts debug verb prehook = { debug; verb; prehook }
let copts_t = 
  let docs = copts_sect in 
  let debug = Arg.(value & flag & info ["debug"] ~docs
        ~doc:"Give only debug output.") in
  let verb =
    let quiet = Quiet, Arg.info ["q"; "quiet"] ~docs
	~doc:"Suppress informational output." in
    let verbose = Verbose, Arg.info ["v"; "verbose"] ~docs
	~doc:"Give verbose output." in 
    Arg.(last & vflag_all [Normal] [quiet; verbose]) 
  in 
  let prehook = Arg.(value & opt (some string) None & info ["prehook"] ~docs
        ~doc:"Specify command to run before this darcs command.")
  in
  Term.(pure copts $ debug $ verb $ prehook)
    
(* Commands *)

let initialize_cmd = 
  let open Term in
  let info = info "initialize" ~sdocs:copts_sect
      ~doc:"make the current directory a repository" ~man:
      ([`S "DESCRIPTION";
        `P "Turns the current directory into a Darcs repository. Any
            existing files and subdirectories become ..."] @ help_secs);
  in
  info, pure initialize $ copts_t $ 
  Arg.(value & opt file Filename.current_dir_name & info ["repodir"]
	 ~docv:"DIR" ~doc:"Run the program in repository directory $(docv).")

let record_cmd =
  let open Term in
  let info = info "record"
      ~doc:"create a patch from unrecorded changes" ~sdocs:copts_sect ~man:
      ([`S "DESCRIPTION";
        `P "Creates a patch from changes in the working tree. If you specify 
	    a set of files ..."] @ help_secs)
  in 
  info,
  pure record $ copts_t $
  Arg.(value & opt (some string) None & info ["m"; "patch-name"] ~docv:"NAME" 
	 ~doc:"Name of the patch.") $
  Arg.(value & opt (some string) None & info ["A"; "author"] ~docv:"EMAIL"
	 ~doc:"Specifies the author's identity.") $
  Arg.(value & flag & info ["a"; "all"]
	 ~doc:"Answer yes to all patches.") $
  Arg.(value & flag & info ["ask-deps"]
	 ~doc:"Ask for extra dependencies.") $
  Arg.(value & (pos_all file) [] & info [] ~docv:"FILE or DIR")

let help_cmd = 
  let open Term in
  let info = info "help" ~sdocs:copts_sect
      ~doc:"display help about darcs and darcs commands" ~man:
      ([`S "DESCRIPTION";
        `P "Without a $(i,TOPIC), prints a list of darcs commands and a short
	    description of each one ..."] @ help_secs)
  in
  info,
  ret (pure help $ copts_t $ Term.choice_names $
  Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" 
	 ~doc:"The topic to get help on: a $(i,COMMAND), `patterns' or 
	       `environment'."))

let cmds = [initialize_cmd; record_cmd; help_cmd]
let no_cmd = Term.(ret (pure help $ copts_t $ Term.choice_names $ pure None))
let info = Term.info "darcs" ~version:"1.6.1" ~sdocs:copts_sect
    ~doc:"a revision control system" ~man:help_secs

let () = match Term.eval_choice info no_cmd cmds with 
| `Error _ -> exit 1 | _ -> exit 0

  
