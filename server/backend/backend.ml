type t = Server_workdirs.t

let cache = Lazy.from_fun Oca_server.Cache.create

let get_compilers logdir =
  let compilers = Server_workdirs.logdir_get_compilers logdir in
  List.sort Intf.Compiler.compare compilers

module Pkg_tbl = Hashtbl.Make (String)

let pkg_update ~pool pkg_tbl logdir comp state pkg =
  let get_content () = Utils.Miou_pool.use pool begin fun () ->
    Server_workdirs.logdir_get_content ~comp ~state ~pkg logdir
  end in
  let content = Intf.Log.create get_content in
  let instances =
    match Pkg_tbl.find_opt pkg_tbl pkg with
    | Some instances -> Intf.Instance.create comp state content :: instances
    | None -> [Intf.Instance.create comp state content]
  in
  Pkg_tbl.replace pkg_tbl pkg instances

let fill_pkgs_from_dir ~pool pkg_tbl logdir comp =
  let good_files = Server_workdirs.goodfiles ~switch:comp logdir in
  let partial_files = Server_workdirs.partialfiles ~switch:comp logdir in
  let bad_files = Server_workdirs.badfiles ~switch:comp logdir in
  let notavailable_files = Server_workdirs.notavailablefiles ~switch:comp logdir in
  let internalfailure_files = Server_workdirs.internalfailurefiles ~switch:comp logdir in
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.Good) good_files;
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.Partial) partial_files;
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.Bad) bad_files;
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.NotAvailable) notavailable_files;
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.InternalFailure) internalfailure_files;
  ()

let add_pkg full_name instances acc =
  let cache = Lazy.force cache in
  let pkg = Intf.Pkg.name (Intf.Pkg.create ~full_name ~instances:[] ~opam:OpamFile.OPAM.empty ~revdeps:0) in (* TODO: Remove this horror *)
  let opam = Oca_server.Cache.get_opam cache pkg in
  let revdeps = Oca_server.Cache.get_revdeps cache full_name in
  Intf.Pkg.create ~full_name ~instances ~opam ~revdeps :: acc

let get_pkgs ~pool ~compilers logdir =
  let pkg_tbl = Pkg_tbl.create 10_000 in
  List.iter (fill_pkgs_from_dir ~pool pkg_tbl logdir) compilers;
  let pkgs = Pkg_tbl.fold add_pkg pkg_tbl [] in
  List.sort Intf.Pkg.compare pkgs

let get_log _ ~logdir ~comp ~state ~pkg =
  let cache = Lazy.force cache in
  let pkgs = Oca_server.Cache.get_pkgs ~logdir cache in
  match List.find_opt (fun p -> String.equal pkg (Intf.Pkg.full_name p)) pkgs with
  | None -> None
  | Some pkg ->
      let is_instance inst =
        Intf.Compiler.equal comp (Intf.Instance.compiler inst) &&
        Intf.State.equal state (Intf.Instance.state inst)
      in
      match List.find_opt is_instance (Intf.Pkg.instances pkg) with
      | None -> None
      | Some instance ->
          let content = Miou.await_exn @@ Intf.Instance.content instance in
          Some content

let get_opams workdir =
  let dir = Server_workdirs.opamsdir workdir in
  let files = Oca_lib.get_files dir in
  let opams = Oca_server.Cache.Opams_cache.empty in
  let opams =
    List.fold_left begin fun opams pkg ->
      let file = Server_workdirs.opamfile ~pkg workdir in
      let content = Utils.with_in (Fpath.to_string file) Utils.read_all in
      let content = try OpamFile.OPAM.read_from_string content with _ -> OpamFile.OPAM.empty in
      Oca_server.Cache.Opams_cache.add pkg content opams
    end opams files
  in
  opams

let get_revdeps workdir =
  let dir = Server_workdirs.revdepsdir workdir in
  let files = Oca_lib.get_files dir in
  let revdeps = Oca_server.Cache.Revdeps_cache.empty in
  let revdeps =
    List.fold_left begin fun revdeps pkg ->
      let file = Server_workdirs.revdepsfile ~pkg workdir in
      let content = Utils.with_in (Fpath.to_string file) Utils.read_all in
      let content = String.split_on_char '\n' content in
      let content = List.hd content in
      let content = int_of_string content in
      Oca_server.Cache.Revdeps_cache.add pkg content revdeps
    end revdeps files
  in
  revdeps

(* TODO: Deduplicate with Server.tcp_server *)
let tcp_server port handler =
  Httpcats.Server.clear ~handler (Unix.ADDR_INET (Unix.inet_addr_loopback, port))

let cache_clear_and_init workdir =
  let cache = Lazy.force cache in
  let pool = Utils.Miou_pool.create 64 (fun () -> ()) in
  Oca_server.Cache.clear_and_init
    cache
    ~pkgs:(fun ~compilers logdir -> get_pkgs ~pool ~compilers logdir)
    ~compilers:(fun logdir -> (get_compilers logdir))
    ~logdirs:(fun () -> Server_workdirs.logdirs workdir)
    ~opams:(fun () -> get_opams workdir)
    ~revdeps:(fun () -> get_revdeps workdir)

let run_action_loop ~conf ~run_trigger f =
  let rec loop previous_action =
    let action =
      try
        let run_interval = Server_configfile.auto_run_interval conf * 60 * 60 in
        let manual_run = Utils.Miou_mvar.take run_trigger in
        if run_interval > 0 then
          let regular_run =
            Miou.async @@ fun () ->
            let () = Miou_unix.sleep (float_of_int run_interval) in
            Check.wait_current_run_to_finish ()
          in
          match Miou.await_first [regular_run; manual_run] with
          | Ok () -> ()
          | Error e -> raise e
        else
          Miou.await_exn manual_run;
        Miou.await_exn previous_action;
        f ()
      with e ->
        let msg = Printexc.to_string e in
        prerr_endline ("Exception raised in action loop: "^msg);
        prerr_endline (Printexc.get_backtrace ());
        previous_action
    in
    loop action
  in
  loop (Miou.async (fun () -> ()))

let start ~debug conf workdir =
  let cache = Lazy.force cache in
  let port = Server_configfile.admin_port conf in
  let on_finished = cache_clear_and_init in
  let run_trigger = Utils.Miou_mvar.create_empty () in
  let callback = Admin.callback ~on_finished ~conf ~run_trigger workdir in
  cache_clear_and_init workdir;
  Mirage_crypto_rng_unix.use_default ();
  let () = Admin.create_admin_key workdir in
  let task () =
    Miou.async (fun () ->
      Miou.await_all [
        Miou.async (fun () -> tcp_server port callback);
        run_action_loop ~conf ~run_trigger (fun () -> Check.run ~debug ~on_finished ~conf cache workdir);
      ] |>
      List.fold_left (fun () -> function
        | Ok () -> ()
        | Error exn -> raise exn
      ) ()
    )
  in
  (workdir, task)
