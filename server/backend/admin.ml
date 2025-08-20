let ( // ) = Fpath.( / )

let with_file_out ~flags file f =
  Utils.with_out file f

let is_username_char = function
  | 'a'..'z' -> true
  | '-' | '_' -> true
  | _ -> false

let get_keyfile workdir username =
  (* TODO: Replace this test by Oca_lib.is_valid_filename ? *)
  if String.is_empty username || not (String.for_all is_username_char username) then
    failwith "Invalid username";
  Server_workdirs.keyfile ~username workdir

let create_userkey workdir username =
  let keyfile = get_keyfile workdir username in
  let key = Mirage_crypto_pk.Rsa.generate ~bits:2048 () in
  let key_pem = X509.Private_key.encode_pem (`RSA key) in
  with_file_out ~flags:[Unix.O_EXCL] (Fpath.to_string keyfile) begin fun chan ->
    Lwt_io.write_line chan key_pem
  end

let create_admin_key workdir =
  let username = Oca_lib.default_admin_name in
  let keyfile = get_keyfile workdir username in
  match await @@ Lwt_unix.file_exists (Fpath.to_string keyfile) with
  | true -> ()
  | false -> create_userkey workdir username

let get_log workdir =
  let ilogdir = Server_workdirs.ilogdir workdir in
  let logs = Oca_lib.get_files ilogdir in
  let logs = List.sort String.compare logs in
  let logfile = Option.get_exn_or "no last log" (List.last_opt logs) in
  let logfile = ilogdir // logfile in
  let fd = Unix.openfile (Fpath.to_string logfile) Unix.[O_RDONLY] 0o644 in
  let fd = Miou_unix.of_file_descr fd in
  let off = ref 0 in
  let rec loop () =
    let is_running = Check.is_running () in
    Miou.yield ();
    let new_off = Unix.lseek (Miou_unix.to_file_descr fd) 0 Unix.SEEK_END in
    if new_off < 0 then
      assert false
    else if !off < new_off then begin
      let _ = Unix.lseek (Miou_unix.to_file_descr fd) !off Unix.SEEK_SET in
      let len = new_off - !off in
      let buf = Bytes.create len in
      let _ = Miou_unix.read fd buf ~off:0 ~len in
      off := new_off;
      (Some (Bytes.to_string buf))
    end else if is_running then begin
      off := new_off;
      let () = Miou_unix.sleep 1. in
      loop ()
    end else
      None
  in
  loop

let admin_action ~on_finished ~conf ~run_trigger workdir body =
  let resp =
    match String.split_on_char '\n' body with
    | ["set-auto-run-interval"; i] ->
        let () = Server_configfile.set_auto_run_interval conf (int_of_string i) in
        (fun () -> None)
    | ["set-processes"; i] ->
        let i = int_of_string i in
        if i < 0 then
          raise (Failure "Cannot set the number of processes to a negative value.")
        else
          let () = Server_configfile.set_processes conf i in
          (fun () -> None)
    | ["add-ocaml-switch";name;switch] ->
        let switch = Intf.Switch.create ~name ~switch in
        let switches = Option.get_or ~default:[] (Server_configfile.ocaml_switches conf) in
        if List.mem ~eq:Intf.Switch.equal switch switches then
          raise (Failure "Cannot have duplicate switches names.")
        else
          let switches = List.sort Intf.Switch.compare (switch :: switches) in
          let () = Server_configfile.set_ocaml_switches conf switches in
          (fun () -> None)
    | ["set-ocaml-switch";name;switch] ->
        let switch = Intf.Switch.create ~name ~switch in
        let switches = Option.get_or ~default:[] (Server_configfile.ocaml_switches conf) in
        let idx, _ = Option.get_exn_or "can't find switch name" (List.find_idx (Intf.Switch.equal switch) switches) in
        let switches = List.set_at_idx idx switch switches in
        let () = Server_configfile.set_ocaml_switches conf switches in
        (fun () -> None)
    | ["rm-ocaml-switch";name] ->
        let switch = Intf.Switch.create ~name ~switch:"(* TODO: remove this shit *)" in
        let switches = Option.get_or ~default:[] (Server_configfile.ocaml_switches conf) in
        let switches = List.remove ~eq:Intf.Switch.equal ~key:switch switches in
        let () = Server_configfile.set_ocaml_switches conf switches in
        (fun () -> None)
    | "set-slack-webhooks"::webhooks ->
        let webhooks = List.map Uri.of_string webhooks in
        let () = Server_configfile.set_slack_webhooks conf webhooks in
        (fun () -> None)
    | ["set-list-command";cmd] ->
        let () = Server_configfile.set_list_command conf cmd in
        (fun () -> None)
    | ["run"] ->
        let () = Utils.Miou_mvar.put run_trigger () in
        (fun () -> None)
    | ["add-user";username] ->
        let () = create_userkey workdir username in
        (fun () -> None)
    | ["clear-cache"] ->
        let () = on_finished workdir in
        (fun () -> None)
    | ["log"] ->
        get_log workdir
    | _ ->
        raise (Failure "Action unrecognized.")
  in
  let stream = Lwt_stream.from (fun () -> Lwt_direct.spawn resp) in
  await @@ Cohttp_lwt_unix.Server.respond ~status:`OK ~body:(`Stream stream) ()

let is_bzero = function
  | '\000' -> true
  | _ -> false

let get_user_key workdir user =
  let keyfile = get_keyfile workdir user in
  let key = Utils.with_in (Fpath.to_string keyfile) Utils.read_all in
    (match X509.Private_key.decode_pem key with
     | Ok `RSA key -> key
     | Ok _ -> failwith "unsupported key type, only RSA supported"
     | Error `Msg m -> failwith ("error decoding key for " ^ user ^ " (must be in PEM format): " ^ m))

let partial_decrypt key msg =
  Mirage_crypto_pk.Rsa.decrypt ~key msg

let rec decrypt key msg =
  let key_size = Mirage_crypto_pk.Rsa.priv_bits key / 8 in
  if String.length msg <= key_size then
    String.drop_while is_bzero (partial_decrypt key msg)
  else
    let msg, next = String.take_drop key_size msg in
    partial_decrypt key msg ^ decrypt key next

let callback ~on_finished ~conf ~run_trigger workdir _conn _req body =
  let body = await @@ Cohttp_lwt.Body.to_string body in
  match String.Split.left ~by:"\n" body with
  | Some (pversion, body) when String.equal Oca_lib.protocol_version pversion ->
      begin match String.Split.left ~by:"\n" body with
      | Some (_, "") ->
          raise (Failure "Empty message")
      | Some (user, body) ->
          let key = get_user_key workdir user in
          let body = decrypt key body in
          begin match String.Split.left ~by:"\n" body with
          | Some (user', body) when String.equal user user' ->
              admin_action ~on_finished ~conf ~run_trigger workdir body
          | Some _ ->
              raise (Failure "Identity couldn't be ensured")
          | None ->
              raise (Failure "Identity check required")
          end
      | None ->
          raise (Failure "Cannot find username")
      end
  | Some (pversion, _) ->
      await @@ Cohttp_lwt_unix.Server.respond_string
        ~status:`Upgrade_required
        ~body:("This server requires opam-health-check protocol version \
                '"^Oca_lib.protocol_version^"' but got '"^pversion^"'. \
                Please upgrade your client.")
        ()
  | None ->
      raise (Failure "Cannot parse request")
