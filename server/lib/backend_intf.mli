type task = unit -> unit Miou.t

module type S = sig
  type t

  val cache : Cache.t Lazy.t

  val get_log : t -> logdir:Server_workdirs.logdir -> comp:Intf.Compiler.t -> state:Intf.State.t -> pkg:string -> string option

  val start : debug: bool -> Server_configfile.t -> Server_workdirs.t -> (t * task * (unit -> unit))
end
