val run :
  debug:bool ->
  on_finished:(Server_workdirs.t -> unit) ->
  conf:Server_configfile.t ->
  Oca_server.Cache.t ->
  Server_workdirs.t ->
  unit Miou.t

val is_running : unit -> bool
val wait_current_run_to_finish : unit -> unit
