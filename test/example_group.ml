(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Cmdliner

let main () = Cmd.eval Testing_cmdliner.sample_group_cmd
let () = if !Sys.interactive then () else exit (main ())
