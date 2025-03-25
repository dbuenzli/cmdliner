open Cmdliner

type mood =
| Panic
| Chill
| Beverage of string
| Nap of int

let mk_opt_arg ~conv ~vopt ~vconv = Arg.Opt (Conv { conv; vopt; vconv })

let options =
  let beverage =
    mk_opt_arg ~conv:Arg.string ~vopt:(Some "coffee") ~vconv:(fun s -> Beverage s)
  in
  let nap =
    mk_opt_arg ~conv:Arg.int ~vopt:(Some 15) ~vconv:(fun i -> Nap i)
  in
  let args =
    Arg.
      [ VFlag Panic, info [ "p"; "panic" ] ~doc:"Panic mode. Not recommended."
      ; VFlag Chill, info [ "c"; "chill" ] ~doc:"Chill mode. Highly recommended."
      ; beverage, info [ "b"; "beverage" ] ~doc:"Pick a drink (default: coffee)."
      ; nap, info [ "n"; "nap" ] ~doc:"Nap time in minutes (default: 15)."
      ]
  in
  Arg.(value & opt_vflag_all [] args)

let run moods =
  let msg =
    List.map (function
      | Panic -> "PANIC!!!"
      | Chill -> "Everything's fine."
      | Beverage b -> "Drinking " ^ b
      | Nap t -> "Nap for " ^ string_of_int t ^ " min.")
      moods
    |> String.concat " | "
  in
  Printf.printf "%s\n" msg;
  Ok ()

let cmd =
  let term = Term.(term_result (const run $ options)) in
  Cmd.v (Cmd.info "moodctl" ~version:"1.0") term

let () = exit (Cmd.eval cmd)