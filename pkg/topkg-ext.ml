(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let ( >>= ) v f = match v with `Ok v -> f v | `Error _ as e -> e  
let ( >>& ) v f = match v with 
| `Ok v -> f v | `Error e -> Printf.eprintf "%s: %s\n%!" Sys.argv.(0) e; exit 1

type 'a result = [ `Ok of 'a | `Error of string ] 

(** Working with files *) 
module File : sig
  val exists : string -> bool
  (** [exists file] is [true] if [file] exists. *) 

  val read : string -> string result
  (** [read file] is [file]'s contents. *) 

  val write : string -> string -> unit result 
  (** [write file content] writes [contents] to [file]. *) 

  val write_subst : string -> (string * string) list -> string -> unit result
  (** [write_subst file vars content] writes [contents] to [file]
      substituting variables of the form [%%ID%%] by their definition.
      The [ID]'s are [List.map fst vars] and their definition content
      is found with [List.assoc]. *)

  val delete : ?maybe:bool -> string -> unit result
  (** [delete maybe file] deletes file [file]. If [maybe] is [true] (defaults
      to false) no error is reported if the file doesn't exist. *)

  val temp : unit -> string result
  (** [temp ()] creates a temporary file and returns its name. The file 
      is destroyed at the end of program execution. *) 
end = struct
  let exists = Sys.file_exists
  let read file = try
    let ic = open_in file in 
    let len = in_channel_length ic in 
    let s = String.create len in 
    really_input ic s 0 len; close_in ic; `Ok s
  with Sys_error e -> `Error e

  let write f s = try 
    let oc = open_out f in 
    output_string oc s; close_out oc; `Ok ()
  with Sys_error e -> `Error e

  let write_subst f vars s = try 
    let oc = open_out f in
    let start = ref 0 in
    let last = ref 0 in 
    let len = String.length s in
    while (!last < len - 4) do
      if not (s.[!last] = '%' && s.[!last + 1] = '%') then incr last else 
      begin 
        let start_subst = !last in
        let last_id = ref (!last + 2) in 
        let stop = ref false in
        while (!last_id < len - 1 && not !stop) do 
          if not (s.[!last_id] = '%' && s.[!last_id + 1] = '%') then begin 
            if s.[!last_id] <> ' ' then (incr last_id) else 
            (stop := true; last := !last_id)
          end else begin 
            let id_start = start_subst + 2 in
            let id = String.sub s (id_start) (!last_id - id_start) in
            try 
              let subst = List.assoc id vars in
              output oc s !start (start_subst - !start); 
              output_string oc subst; 
              stop := true;
              start := !last_id + 2; 
              last := !last_id + 2;
            with Not_found -> 
              stop := true; 
              last := !last_id
          end
        done
      end
    done;
    output oc s !start (len - !start); close_out oc; `Ok () 
  with Sys_error e -> `Error e
  
  let delete ?(maybe = false) file = try
    if maybe && not (exists file) then `Ok () else
    `Ok (Sys.remove file) 
  with Sys_error e -> `Error e
                        
  let temp () = try 
    let f = Filename.temp_file (Filename.basename Sys.argv.(0)) "topkg" in
    at_exit (fun () -> ignore (delete f)); `Ok f
  with Sys_error e -> `Error e
end

(** Working with directories. *) 
module Dir : sig
  val exists : string -> bool
  (** [exists dir] is [true] if directory [dir] exists. *) 

  val change_cwd : string -> unit result 
  (** [change_cwd dir] changes the current working directory to [dir]. *)

  val fold_files_rec : ?skip:string list -> (string -> 'a -> 'a result) -> 
    'a -> string list -> 'a result
  (** [fold_files_rec skip f acc paths] folds [f] over the files 
      found in [paths]. Files and directories whose suffix matches an 
      element of [skip] are skipped. *)
end = struct
  let exists dir = Sys.file_exists dir && Sys.is_directory dir
  let change_cwd dir = try `Ok (Sys.chdir dir) with Sys_error e -> `Error e
  let fold_files_rec ?(skip = []) f acc paths = 
    let is_dir d = try Sys.is_directory d with Sys_error _ -> false in
    let readdir d = try Array.to_list (Sys.readdir d) with Sys_error _ -> [] in
    let keep p = not (List.exists (fun s -> Filename.check_suffix p s) skip) in
    let process acc file = match acc with 
    | `Error _ as e -> e 
    | `Ok acc -> f file acc 
    in
    let rec aux f acc = function
    | (d :: ds) :: up -> 
        let paths = List.rev_map (Filename.concat d) (readdir d) in
        let paths = List.find_all keep paths in
        let dirs, files = List.partition is_dir paths in
        begin match List.fold_left process acc files with 
        | `Error _ as e -> e
        | `Ok _ as acc -> aux f acc (dirs :: ds :: up)
        end
    | [] :: [] -> acc
    | [] :: up -> aux f acc up
    | _ -> assert false
    in
    let paths = List.find_all keep paths in
    let dirs, files = List.partition is_dir paths in
    let acc = List.fold_left process (`Ok acc) files in
    aux f acc (dirs :: []) 
end

(** Command invocation. *) 
module Cmd : sig
  val exec : string -> unit result
  (** [exec cmd] executes [cmd]. *) 

  val exec_hook : string option -> unit result 
  (** [exec_hook args] is [exec ("ocaml " ^ "args")] if [args] is some. *)

  val read : string -> string result
  (** [read cmd] executes [cmd] and returns the contents of its stdout. *) 
end = struct
  let exec cmd =
    let code = Sys.command cmd in 
    if code = 0 then `Ok () else
    `Error (Printf.sprintf "invocation `%s' exited with %d" cmd code)

  let exec_hook args = match args with 
  | None -> `Ok () 
  | Some args -> exec (Printf.sprintf "ocaml %s" args)

  let read cmd =
    File.temp () >>= fun file ->
    exec (Printf.sprintf "%s > %s" cmd file) >>= fun () ->
    File.read file >>= fun v ->
    `Ok v
end

(** Variable substitution. *)
module Vars : sig
  val subst : skip:string list -> vars:(string * string) list -> 
    dir:string -> unit result
  (** [subst skip vars dir] substitutes [vars] in all files 
      in [dir] except those that are [skip]ped (see {!Dir.fold_files_rec}). *)
      
  val get : string -> (string * string) list -> string result 
  (** [get v] lookup variable [v] in [vars]. Returns an error if [v] is 
      absent or if it is the empty string. *)

end = struct
  let subst ~skip ~vars ~dir =
    let subst f () = 
      File.read f >>= fun contents -> 
      File.write_subst f vars contents >>= fun () -> `Ok ()
    in
    Dir.fold_files_rec ~skip subst () [dir]

  let get v vars = 
    let v = try List.assoc v vars with Not_found -> "" in 
    if v <> "" then `Ok v else
    `Error (Printf.sprintf "empty or undefined variable %s in Config.vars" v)
end

(** Git invocations. *) 
module Git : sig
  val describe : ?chop_v:bool -> string -> string 
  (** [describe chop_v branch] invokes [git describe branch]. If [chop_v]
      is [true] (defaults to [false]) an initial ['v'] in the result 
      is chopped. *)
end = struct
  let describe ?(chop_v = false) branch =
    if not (Dir.exists ".git") then "not-a-git-checkout" else
    Cmd.read (Printf.sprintf "git describe %s" branch) >>& fun d ->
    let len = String.length d in
    if chop_v && len > 0 && d.[0] = 'v' then String.sub d 1 (len - 2) else 
    String.sub d 0 (len - 1) (* remove \n *)
end

(** Default configuration. *) 
module Config_default : sig
  val subst_skip : string list
  (** [subst_skip] is a list of suffixes that are automatically
      skipped during variable substitution. *)

  val vars : (string * string) list 
  (** [vars] is the list of variables to substitute, empty. *) 

  val git_hook : string option
  (** [git_start_hook] is an ocaml script to invoke before a git package 
      build, after variable substitution occured. *) 

  val distrib_remove : string list
  (** [distrib_remove] is a list of files to remove before making 
      the distributino tarball. *) 

  val distrib_hook : string option 
  (** [distrib_hook] is an ocaml script to invoke before trying 
      to build the distribution. *)

  val www_demos : string list 
  (** [www_demos] is a list of build targets that represent single page
      js_of_ocaml demo. *)
end = struct
  let subst_skip = [".git"; ".png"; ".jpeg"; ".otf"; ".ttf"; ".pdf" ]
  let vars = []
  let git_hook = None
  let distrib_remove = [".git"; ".gitignore"; "build"]
  let distrib_hook = None
  let www_demos = [] 
end


(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
