module Make (_ : Backend_intf.S) : sig
  val main : debug:bool -> workdir:string -> unit Lwt.t
end
