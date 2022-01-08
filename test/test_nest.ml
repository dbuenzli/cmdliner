
open Cmdliner

let kind =
  let doc = "Kind of entity" in
  Arg.(value & opt (some string) None & info ["k";"kind"] ~doc)

let speed =
  let doc = "Movement $(docv) in m/s" in
  Arg.(value & opt int 2 & info ["speed"] ~doc ~docv:"SPEED")

let birds =
  let bird =
    let doc = "Use $(docv) specie." in
    Arg.(value & pos 0 string "pigeon" & info [] ~doc ~docv:"BIRD")
  in
  let fly =
    let info = Cmd.info "fly" ~doc:"Fly birds." in
    Cmd.v info Term.(const (fun n v -> ()) $ bird $ speed)
  in
  let land' =
    let info = Cmd.info "land" ~doc:"Land birds." in
    Cmd.v info Term.(const (fun n -> ()) $ bird)
  in
  let info = Cmd.info "birds" ~doc:"Operate on birds." in
  Cmd.group ~default:Term.(const (fun k -> ()) $ kind) info [fly; land']

let mammals =
  let info = Cmd.info "mammals" ~doc:"Operate on mammals." in
  Cmd.v info Term.(const (fun () -> ()) $ const ())

let fishs =
  let name' =
    let doc = "Use fish named $(docv)." in
    Arg.(value & pos 0 (some string) None & info [] ~doc ~docv:"NAME")
  in
  let info = Cmd.info "fishs" ~doc:"Operate on fishs." in
  Cmd.v info Term.(const (fun n -> ()) $ name')

let cmd =
  let info = Cmd.info "test_nest" ~version:"X.Y.Z" in
  Cmd.group info [birds; mammals; fishs]

let () = exit (Cmd.eval cmd)
