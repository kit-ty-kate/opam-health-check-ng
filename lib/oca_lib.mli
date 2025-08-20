val list_map_cube : ('a -> 'a -> 'b) -> 'a list -> 'b list

val is_valid_filename : string -> bool
val char_is_docker_compatible : char -> bool

val read_line_opt : Miou_unix.file_descr -> string option

val write : Miou_unix.file_descr -> string -> unit
val write_line : Miou_unix.file_descr -> string -> unit

val with_atomic_file_out :
  ext:string ->
  Fpath.t ->
  (out_channel -> unit) ->
  unit

val with_file :
  Unix.open_flag list ->
  Unix.file_perm ->
  string ->
  (Miou_unix.file_descr -> 'a) ->
  'a

val exec :
  timeout:float ->
  cidfile:string option ->
  stdin:Utils.Miou_process.redirection ->
  stdout:Miou_unix.file_descr ->
  stderr:Miou_unix.file_descr ->
  string list ->
  (unit, unit) result

val get_files : Fpath.t -> string list
val scan_dir : Fpath.t -> string list
val scan_tpxz_archive : Fpath.t -> string list
val random_access_tpxz_archive : file:string -> Fpath.t -> string
val compress_tpxz_archive : cwd:Fpath.t -> directories:string list -> Fpath.t -> unit
val ugrep_dir : switch:string -> regexp:string -> cwd:Fpath.t -> string list
val ugrep_tpxz : switch:string -> regexp:string -> archive:Fpath.t -> string list
val mkdir_p : Fpath.t -> unit
val rm_rf : Fpath.t -> unit

val with_temp_dir : (string -> 'a) -> 'a

type timer

val timer_start : unit -> timer
val timer_log : timer -> Miou_unix.file_descr -> string -> unit

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
