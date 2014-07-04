#!/usr/bin/env ocaml 
#directory "pkg"
#use "config.ml" 

(* This is only for git checkout builds, it can be ignored
   for distribution builds. *)

let () = 
  if Dir.exists ".git" then begin
    Vars.subst ~skip:Config.subst_skip ~vars:Config.vars ~dir:"." 
    >>& fun () -> Cmd.exec_hook Config.git_hook 
    >>& fun () -> ()
  end

