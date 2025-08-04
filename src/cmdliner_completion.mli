(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

val output :
  out_ppf:Format.formatter ->
  err_ppf:Format.formatter ->
  Cmdliner_info.Eval.t ->
  Cmdliner_info.Arg_info.Set.t ->
  'a Cmdliner_cmd.t -> Cmdliner_info.Complete.t -> unit
