let (//) = Fpath.(/)

let with_atomic_file_out ~ext file f =
  let tmp_file = Fpath.add_ext ext file in
  IO.with_out (Fpath.to_string tmp_file) f;
  Unix.rename (Fpath.to_string tmp_file) (Fpath.to_string file)

let rec list_map_cube f = function
  | x::(_::_ as xs) -> List.map (f x) xs @ list_map_cube f xs
  | [_] | [] -> []

let is_valid_filename file =
  not begin
    String.is_empty file ||
    Fpath.is_rel_seg file ||
    not (Fpath.is_seg file)
  end

let char_is_docker_compatible = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
  (* TODO: Add more *)
  | _ -> false

let get_files dirname =
  let%lwt dir = Lwt_unix.opendir (Fpath.to_string dirname) in
  let rec aux files =
    try%lwt
      let%lwt file = Lwt_unix.readdir dir in
      if Fpath.is_rel_seg file then
        aux files
      else
        aux (file :: files)
    with
    | End_of_file -> Lwt.return files
  in
  let%lwt files = aux [] in
  let%lwt () = Lwt_unix.closedir dir in
  Lwt.return files

let rec scan_dir ~full_path dirname =
  let%lwt files = get_files full_path in
  Lwt_list.fold_left_s (fun acc file ->
    let full_path = Fpath.add_seg full_path file in
    let file = Fpath.normalize (Fpath.add_seg dirname file) in
    match%lwt Lwt_unix.stat (Fpath.to_string full_path) with
    | {Unix.st_kind = Unix.S_DIR; _} ->
        let%lwt files = scan_dir ~full_path file in
        Lwt.return (Fpath.to_string (Fpath.add_seg file "") :: files @ acc)
    | {Unix.st_kind = Unix.S_REG; _} ->
        Lwt.return (Fpath.to_string file :: acc)
    | {Unix.st_kind = Unix.(S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK); _} ->
        assert false
  ) [] files

let scan_dir dirname = scan_dir ~full_path:dirname (Fpath.v ".")

let read_line_opt fd =
  let buffer = Buffer.create 256 in
  let tmp_buf = Bytes.create 1 in
  let rec aux () =
    match%lwt Lwt_unix.read fd tmp_buf 0 1 with
    | 0 -> Lwt.return None
    | 1 ->
        begin match Bytes.get tmp_buf 0 with
        | '\n' -> Lwt.return (Some (Buffer.contents buffer))
        | c -> Buffer.add_char buffer c; aux ()
        end
    | _ -> assert false
  in
  aux ()

let write fd str =
  let rec aux idx len =
    let%lwt bytes_written = Lwt_unix.write_string fd str idx len in
    match len - bytes_written with
    | 0 -> Lwt.return_unit
    | new_len -> aux (idx + bytes_written) new_len
  in
  aux 0 (String.length str)

let write_line fd str =
  write fd (str^"\n")

let with_file flags mode filename f =
  let%lwt fd = Lwt_unix.openfile filename flags mode in
  Lwt.finalize (fun () -> f fd) (fun () -> Lwt_unix.close fd)

let exec ~timeout ~ciddir ~stdin ~stdout ~stderr cmd =
  let cmd, cidfile = match ciddir with
    | None -> (cmd, None)
    | Some ciddir ->
        let cidfile = Fpath.to_string (ciddir//"cidfile") in
        (cmd @ ["--cidfile";cidfile], Some cidfile)
  in
  let stdout = `FD_copy (Lwt_unix.unix_file_descr stdout) in
  let stderr = `FD_copy (Lwt_unix.unix_file_descr stderr) in
  (* TODO: maybe to factorize with pread below *)
  Lwt_process.with_process_none ~stdin ~stdout ~stderr ("", Array.of_list cmd) (fun proc ->
    let proc' =
      match%lwt proc#close with
      | Unix.WEXITED 0 ->
          Lwt.return (Ok ())
      | Unix.WEXITED n ->
          let cmd = String.concat " " cmd in
          prerr_endline ("Command '"^cmd^"' failed (exit status: "^string_of_int n^")");
          Lwt.return (Error ())
      | Unix.WSIGNALED n | Unix.WSTOPPED n ->
          let cmd = String.concat " " cmd in
          prerr_endline ("Command '"^cmd^"' killed by a signal (n°"^string_of_int n^")");
          Lwt.return (Error ())
    in
    (* NOTE: e.g. any processes shouldn't take more than 2 hours *)
    let timeout =
      let hours = timeout in
      let%lwt () = Lwt_unix.sleep (hours *. 60.0 *. 60.0) in
      let cmd = String.concat " " cmd in
      (* TODO: show errors properly in stderr and on the debug console (same for the errors above) *)
      prerr_endline ("Command '"^cmd^"' timed-out ("^string_of_float hours^" hours)");
      let%lwt () =
        match cidfile with
        | None -> Lwt.return_unit
        | Some cidfile ->
            let container_id = IO.with_in cidfile (IO.read_all ~size:128) in
            match%lwt
              Lwt_process.exec ~stdin:`Close ~stdout:stderr ~stderr
                ("", Array.of_list ["docker";"kill";"-s";"KILL";container_id])
            with
            | Unix.WEXITED 0 -> Lwt.return ()
            | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
                prerr_endline "docker kill failed";
                Lwt.return ()
      in
      proc#terminate;
      Lwt.return (Error ())
    in
    Lwt.pick [timeout; proc']
  )

let pread ?cwd ?exit1 ~timeout cmd f =
  Lwt_process.with_process_in ?cwd ~timeout ~stdin:`Close ("", Array.of_list cmd) begin fun proc ->
    let%lwt res = f proc#stdout in
    match%lwt proc#close with
    | Unix.WEXITED n ->
        begin match n, exit1 with
        | 0, _ ->
            Lwt.return res
        | 1, Some default_val ->
            Lwt.return default_val
        | _, _ ->
            let cmd = String.concat " " cmd in
            prerr_endline ("Command '"^cmd^"' failed (exit status: "^string_of_int n^")");
            Lwt.fail (Failure "process failure")
        end
    | Unix.WSIGNALED n | Unix.WSTOPPED n ->
        let cmd = String.concat " " cmd in
        prerr_endline ("Command '"^cmd^"' killed by a signal (n°"^string_of_int n^")");
        Lwt.fail (Failure "process failure")
  end

let read_unordered_lines c =
  let rec aux acc =
    match%lwt Lwt_io.read_line_opt c with
    | None -> Lwt.return acc (* Note: We don't care about the line ordering *)
    | Some line -> aux (line :: acc)
  in
  aux []

let scan_tpxz_archive archive =
  pread ~timeout:60. ["pixz"; "-l"; Fpath.to_string archive] read_unordered_lines

let random_access_tpxz_archive ~file archive =
  let file = Filename.quote file in
  let archive = Filename.quote (Fpath.to_string archive) in
  pread ~timeout:60. ["sh"; "-c"; "pixz -i "^archive^" -x "^file^" | tar -xO"] (Lwt_io.read ?count:None)

let compress_tpxz_archive ~cwd ~directories archive =
  let cwd = Fpath.to_string cwd in
  let timeout = 3. *. 3600. in (* 3 hours *)
  pread ~timeout ~cwd ("tar" :: "-Ipixz" :: "-cf" :: Fpath.to_string archive :: directories) begin fun _ ->
    (* TODO: Do not use pread *)
    Lwt.return ()
  end

let ugrep_dir ~switch ~regexp ~cwd =
  let cwd = Fpath.to_string cwd in
  pread ~timeout:60. ~cwd ~exit1:[] ["ugrep"; "-Rl"; "--include="^switch^"/**"; "--regexp="^regexp; "."] read_unordered_lines

let ugrep_tpxz ~switch ~regexp ~archive =
  let switch = Filename.quote switch in
  let regexp = Filename.quote regexp in
  let archive = Filename.quote (Fpath.to_string archive) in
  pread ~timeout:60. ~exit1:[] ["sh"; "-c"; "pixz -i "^archive^" -x "^switch^" | ugrep -zl --format='%z%~' --regexp="^regexp] read_unordered_lines

let mkdir_p dir =
  let rec aux base = function
    | [] ->
        Lwt.return_unit
    | x::xs ->
        let dir = Fpath.add_seg base x in
        let%lwt [@ocaml.warning "-fragile-match"] () =
          try%lwt
            Lwt_unix.mkdir (Fpath.to_string dir) 0o750
          with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit
        in
        aux dir xs
  in
  match Fpath.segs dir with
  | ""::dirs -> aux Fpath.(v dir_sep) dirs
  | dirs -> aux (Fpath.v Filename.current_dir_name) dirs

let rec rm_rf dirname =
  let%lwt dir = Lwt_unix.opendir (Fpath.to_string dirname) in
  begin
    let rec rm_files () =
      match%lwt Lwt_unix.readdir dir with
      | "." | ".." -> rm_files ()
      | file ->
          let file = dirname // file in
          let%lwt stat = Lwt_unix.stat (Fpath.to_string file) in
          let%lwt () =
            match stat.Unix.st_kind with
            | Unix.S_DIR -> rm_rf file
            | Unix.S_REG -> Lwt_unix.unlink (Fpath.to_string file)
            | Unix.(S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK) -> assert false
          in
          rm_files ()
    in
    try%lwt rm_files () with End_of_file -> Lwt.return_unit
  end
  [%lwt.finally
    let%lwt () = Lwt_unix.closedir dir in
    Lwt_unix.rmdir (Fpath.to_string dirname)
  ]

type timer = float ref

let timer_start () =
  ref (Unix.time ())

let timer_log timer c msg =
  let start_time = !timer in
  let end_time = Unix.time () in
  let time_span = end_time -. start_time in
  let%lwt () = write_line c ("Done. "^msg^" took: "^string_of_float time_span^" seconds") in
  timer := Unix.time ();
  Lwt.return_unit

let protocol_version = "2"
let default_server_name = "default" (* TODO: Just make it random instead?! *)
let default_html_port = "8080"
let default_public_url = "http://check.ocamllabs.io"
let default_admin_port = "9999"
let default_admin_name = "admin"
let default_auto_run_interval = 0 (* in hours (0 disables auto-run) *)
let default_processes = Domain.recommended_domain_count ()
let default_list_command = "opam list --available --installable --short --all-versions"
let localhost = "localhost"
