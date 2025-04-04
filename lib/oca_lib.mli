val list_map_cube : ('a -> 'a -> 'b) -> 'a list -> 'b list

val is_valid_filename : string -> bool
val char_is_docker_compatible : char -> bool

val read_line_opt : Lwt_unix.file_descr -> string option Lwt.t

val write : Lwt_unix.file_descr -> string -> unit Lwt.t
val write_line : Lwt_unix.file_descr -> string -> unit Lwt.t

val with_atomic_file_out :
  ext:string ->
  Fpath.t ->
  (out_channel -> unit) ->
  unit

val with_file :
  Lwt_unix.open_flag list ->
  Lwt_unix.file_perm ->
  string ->
  (Lwt_unix.file_descr -> 'a Lwt.t) ->
  'a Lwt.t

val exec :
  timeout:float ->
  ciddir:Fpath.t option ->
  stdin:Lwt_process.redirection ->
  stdout:Lwt_unix.file_descr ->
  stderr:Lwt_unix.file_descr ->
  string list ->
  (unit, unit) result Lwt.t

val get_files : Fpath.t -> string list Lwt.t
val scan_dir : Fpath.t -> string list Lwt.t
val scan_tpxz_archive : Fpath.t -> string list Lwt.t
val random_access_tpxz_archive : file:string -> Fpath.t -> string Lwt.t
val compress_tpxz_archive : cwd:Fpath.t -> directories:string list -> Fpath.t -> unit Lwt.t
val ugrep_dir : switch:string -> regexp:string -> cwd:Fpath.t -> string list Lwt.t
val ugrep_tpxz : switch:string -> regexp:string -> archive:Fpath.t -> string list Lwt.t
val mkdir_p : Fpath.t -> unit Lwt.t
val rm_rf : Fpath.t -> unit Lwt.t

type timer

val timer_start : unit -> timer
val timer_log : timer -> Lwt_unix.file_descr -> string -> unit Lwt.t

val protocol_version : string
val default_server_name : string
val default_html_port : string
val default_public_url : string
val default_admin_port : string
val default_admin_name : string
val default_auto_run_interval : int
val default_processes : int
val default_list_command : string
val localhost : string
