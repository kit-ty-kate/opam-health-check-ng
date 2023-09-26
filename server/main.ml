module Server = Oca_server.Server.Make (Backend)

let main debug workdir = Lwt_main.run (Server.main ~debug ~workdir)

(* Command-line parsing *)

module Arg = Cmdliner.Arg
module Cmd = Cmdliner.Cmd
module Term = Cmdliner.Term

let ( $ ) = Term.( $ )
let ( & ) = Arg.( & )

let debug = Arg.value & Arg.flag & Arg.info ["debug"]

let workdir =
  Arg.required & Arg.pos 0 (Arg.some Arg.string) None & Arg.info ~docv:"WORKDIR" []

let term = Term.const main $ debug $ workdir

let info =
  Cmd.info
    ~doc:"A server to check for broken opam packages."
    ~man:[`P "This program takes a work directory where every files created \
              are stored. This includes logs, config file and user private \
              keys."]
    ~version:Config.version
    Config.name

let () =
  Memtrace.trace_if_requested ~context:"opam-health-check" ();
  exit (Cmd.eval (Cmd.v info term))
