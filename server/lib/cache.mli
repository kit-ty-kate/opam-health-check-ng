module Opams_cache : Map.S with type key = string
module Revdeps_cache : Map.S with type key = string

type t

val create : unit -> t

val clear_and_init :
  t ->
  pkgs:(compilers:Intf.Compiler.t list -> Server_workdirs.logdir -> Intf.Pkg.t list) ->
  compilers:(Server_workdirs.logdir -> Intf.Compiler.t list) ->
  logdirs:(unit -> Server_workdirs.logdir list) ->
  opams:(unit -> OpamFile.OPAM.t Opams_cache.t) ->
  revdeps:(unit -> int Revdeps_cache.t) ->
  unit

val get_latest_logdir : t -> Server_workdirs.logdir option
val get_html : conf:Server_configfile.t -> t -> Html.query -> Server_workdirs.logdir -> string
val get_logdirs : t -> Server_workdirs.logdir list
val get_pkgs : logdir:Server_workdirs.logdir -> t -> Intf.Pkg.t list
val get_compilers : logdir:Server_workdirs.logdir -> t -> Intf.Compiler.t list
val get_opam : t -> string -> OpamFile.OPAM.t
val get_revdeps : t -> string -> int
val get_html_diff : conf:Server_configfile.t -> old_logdir:Server_workdirs.logdir -> new_logdir:Server_workdirs.logdir -> t -> string
val get_html_diff_list : t -> string
val get_html_run_list : t -> string
val get_json_latest_packages : t -> string
