val create_admin_key : Server_workdirs.t -> unit

val callback :
  on_finished:(Server_workdirs.t -> unit) ->
  conf:Server_configfile.t ->
  run_trigger:Miou_sync.Trigger.t ->
  Server_workdirs.t ->
  _ ->
  [< `V1 of H1.Reqd.t | `V2 of H2.Reqd.t ] ->
  unit
