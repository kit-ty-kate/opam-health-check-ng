let await = Lwt_direct.await
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
  let dir = await @@ Lwt_unix.opendir (Fpath.to_string dirname) in
  let rec aux files =
    try
      let file = await @@ Lwt_unix.readdir dir in
      if Fpath.is_rel_seg file then
        aux files
      else
        aux (file :: files)
    with
    | End_of_file -> files
  in
  let files = aux [] in
  let () = await @@ Lwt_unix.closedir dir in
  files

let rec scan_dir ~full_path dirname =
  let files = get_files full_path in
  List.fold_left (fun acc file ->
    let full_path = Fpath.add_seg full_path file in
    let file = Fpath.normalize (Fpath.add_seg dirname file) in
    match await @@ Lwt_unix.stat (Fpath.to_string full_path) with
    | {Unix.st_kind = Unix.S_DIR; _} ->
        let files = scan_dir ~full_path file in
        Fpath.to_string (Fpath.add_seg file "") :: files @ acc
    | {Unix.st_kind = Unix.S_REG; _} ->
        Fpath.to_string file :: acc
    | {Unix.st_kind = Unix.(S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK); _} ->
        assert false
  ) [] files

let scan_dir dirname = scan_dir ~full_path:dirname (Fpath.v ".")

let read_line_opt fd =
  let buffer = Buffer.create 256 in
  let tmp_buf = Bytes.create 1 in
  let rec aux () =
    match await @@ Lwt_unix.read fd tmp_buf 0 1 with
    | 0 -> None
    | 1 ->
        begin match Bytes.get tmp_buf 0 with
        | '\n' -> Some (Buffer.contents buffer)
        | c -> Buffer.add_char buffer c; aux ()
        end
    | _ -> assert false
  in
  aux ()

let write fd str =
  let rec aux idx len =
    let bytes_written = await @@ Lwt_unix.write_string fd str idx len in
    match len - bytes_written with
    | 0 -> ()
    | new_len -> aux (idx + bytes_written) new_len
  in
  aux 0 (String.length str)

let write_line fd str =
  write fd (str^"\n")

let with_file flags mode filename f =
  let fd = await @@ Lwt_unix.openfile filename flags mode in
  Fun.protect (fun () -> f fd) ~finally:(fun () -> await @@ Lwt_unix.close fd)

let exec ~timeout ~cidfile ~stdin ~stdout ~stderr cmd =
  let stdout = `FD_copy (Lwt_unix.unix_file_descr stdout) in
  let stderr = `FD_copy (Lwt_unix.unix_file_descr stderr) in
  (* TODO: maybe to factorize with pread below *)
  await @@ Lwt_process.with_process_none ~stdin ~stdout ~stderr ("", Array.of_list cmd) (fun proc ->
    Lwt_direct.spawn @@ fun () ->
    let proc' =
      Lwt_direct.spawn @@ fun () ->
      match await @@ proc#close with
      | Unix.WEXITED 0 ->
          (Ok ())
      | Unix.WEXITED n ->
          let cmd = String.concat " " cmd in
          prerr_endline ("Command '"^cmd^"' failed (exit status: "^string_of_int n^")");
          (Error ())
      | Unix.WSIGNALED n | Unix.WSTOPPED n ->
          let cmd = String.concat " " cmd in
          prerr_endline ("Command '"^cmd^"' killed by a signal (n°"^string_of_int n^")");
          (Error ())
    in
    (* NOTE: e.g. any processes shouldn't take more than 2 hours *)
    let timeout =
      Lwt_direct.spawn @@ fun () ->
      let hours = timeout in
      let () = await @@ Lwt_unix.sleep (hours *. 60.0 *. 60.0) in
      let cmd = String.concat " " cmd in
      (* TODO: show errors properly in stderr and on the debug console (same for the errors above) *)
      prerr_endline ("Command '"^cmd^"' timed-out ("^string_of_float hours^" hours)");
      let () =
        match cidfile with
        | None -> ()
        | Some cidfile ->
            let container_id = IO.with_in cidfile (IO.read_all ~size:128) in
            match await @@
              Lwt_process.exec ~stdin:`Close ~stdout:stderr ~stderr
                ("", Array.of_list ["docker";"kill";"-s";"KILL";container_id])
            with
            | Unix.WEXITED 0 -> ()
            | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
                prerr_endline "docker kill failed"
      in
      proc#terminate;
      Error ()
    in
    await @@ Lwt.pick [timeout; proc']
  )

let pread ?cwd ?exit1 ~timeout cmd f =
  await @@ Lwt_process.with_process_in ?cwd ~timeout ~stdin:`Close ("", Array.of_list cmd) begin fun proc ->
    Lwt_direct.spawn @@ fun () ->
    let res = f proc#stdout in
    match await @@ proc#close with
    | Unix.WEXITED n ->
        begin match n, exit1 with
        | 0, _ ->
            res
        | 1, Some default_val ->
            default_val
        | _, _ ->
            let cmd = String.concat " " cmd in
            prerr_endline ("Command '"^cmd^"' failed (exit status: "^string_of_int n^")");
            raise (Failure "process failure")
        end
    | Unix.WSIGNALED n | Unix.WSTOPPED n ->
        let cmd = String.concat " " cmd in
        prerr_endline ("Command '"^cmd^"' killed by a signal (n°"^string_of_int n^")");
        raise (Failure "process failure")
  end

let read_unordered_lines c =
  let rec aux acc =
    match await @@ Lwt_io.read_line_opt c with
    | None -> acc (* Note: We don't care about the line ordering *)
    | Some line -> aux (line :: acc)
  in
  aux []

let scan_tpxz_archive archive =
  pread ~timeout:60. ["pixz"; "-l"; Fpath.to_string archive] read_unordered_lines

let random_access_tpxz_archive ~file archive =
  let file = Filename.quote file in
  let archive = Filename.quote (Fpath.to_string archive) in
  pread ~timeout:60. ["sh"; "-c"; "pixz -i "^archive^" -x "^file^" | tar -xO"] (fun ic -> await @@ Lwt_io.read ?count:None ic)

let compress_tpxz_archive ~cwd ~directories archive =
  let cwd = Fpath.to_string cwd in
  let timeout = 3. *. 3600. in (* 3 hours *)
  pread ~timeout ~cwd ("tar" :: "-Ipixz" :: "-cf" :: Fpath.to_string archive :: directories) begin fun _ ->
    (* TODO: Do not use pread *)
    ()
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
        ()
    | x::xs ->
        let dir = Fpath.add_seg base x in
        let () =
          try
            await @@ Lwt_unix.mkdir (Fpath.to_string dir) 0o750
          with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
        in
        aux dir xs
  in
  match Fpath.segs dir with
  | ""::dirs -> aux Fpath.(v dir_sep) dirs
  | dirs -> aux (Fpath.v Filename.current_dir_name) dirs

let rec rm_rf dirname =
  let dir = await @@ Lwt_unix.opendir (Fpath.to_string dirname) in
  Fun.protect (fun () ->
    let rec rm_files () =
      match await @@ Lwt_unix.readdir dir with
      | "." | ".." -> rm_files ()
      | file ->
          let file = dirname // file in
          let stat = await @@ Lwt_unix.stat (Fpath.to_string file) in
          let () =
            match stat.Unix.st_kind with
            | Unix.S_DIR -> rm_rf file
            | Unix.S_REG -> await @@ Lwt_unix.unlink (Fpath.to_string file)
            | Unix.(S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK) -> assert false
          in
          rm_files ()
    in
    try rm_files () with End_of_file -> ()
  ) ~finally:(fun () ->
    let () = await @@ Lwt_unix.closedir dir in
    await @@ Lwt_unix.rmdir (Fpath.to_string dirname)
  )

type timer = float ref

let timer_start () =
  ref (Unix.time ())

let timer_log timer c msg =
  let start_time = !timer in
  let end_time = Unix.time () in
  let time_span = end_time -. start_time in
  let () = write_line c ("Done. "^msg^" took: "^string_of_float time_span^" seconds") in
  timer := Unix.time ();
  ()

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
