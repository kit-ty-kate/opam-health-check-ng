module Make (Backend : Backend_intf.S) = struct
  let option_to_string = function
    | None -> ""
    | Some s -> s

  let get_query_param_list uri name =
    Uri.query uri |>
    List.fold_left begin fun acc (k, v) ->
      if String.equal k name then v @ acc else acc
    end [] |>
    List.rev

  let parse_raw_query logdir uri =
    let cache = Lazy.force Backend.cache in
    let checkbox_default def = if List.is_empty (Uri.query uri) then def else false in
    let compilers = get_query_param_list uri "comp" in
    let show_available = get_query_param_list uri "available" in
    let show_failures_only = option_to_string (Uri.get_query_param uri "show-failures-only") in
    let show_failures_only = if String.is_empty show_failures_only then checkbox_default false else bool_of_string show_failures_only in
    let show_only = get_query_param_list uri "show-only" in
    let show_diff_only = option_to_string (Uri.get_query_param uri "show-diff-only") in
    let show_diff_only = if String.is_empty show_diff_only then checkbox_default false else bool_of_string show_diff_only in
    let show_latest_only = option_to_string (Uri.get_query_param uri "show-latest-only") in
    let show_latest_only = if String.is_empty show_latest_only then checkbox_default true else bool_of_string show_latest_only in
    let sort_by_revdeps = option_to_string (Uri.get_query_param uri "sort-by-revdeps") in
    let sort_by_revdeps = if String.is_empty sort_by_revdeps then checkbox_default false else bool_of_string sort_by_revdeps in
    let maintainers = option_to_string (Uri.get_query_param uri "maintainers") in
    let maintainers = if String.is_empty maintainers then None else Some maintainers in
    let maintainers = (option_to_string maintainers, Option.map (Re.Posix.compile_pat ~opts:[`ICase]) maintainers) in
    let logsearch = option_to_string (Uri.get_query_param uri "logsearch") in
    let logsearch = if String.is_empty logsearch then None else Some logsearch in
    let logsearch' =
      Option.map2 begin fun re comp ->
        (Re.Posix.compile_pat ~opts:[`Newline] re, Intf.Compiler.from_string comp)
      end logsearch (Uri.get_query_param uri "logsearch_comp")
    in
    let logsearch = (option_to_string logsearch, logsearch') in
    let available_compilers = Cache.get_compilers ~logdir cache in
    let compilers = match compilers with
      | [] -> available_compilers
      | compilers -> List.map Intf.Compiler.from_string compilers
    in
    let show_available = match show_available with
      | [] -> compilers
      | show_available -> List.map Intf.Compiler.from_string show_available
    in
    let show_only = match show_only with
      | [] when show_failures_only -> [Intf.State.Bad; Intf.State.Partial]
      | [] -> Intf.State.all
      | show_only -> List.map Intf.State.from_string show_only
    in
    {
      Html.available_compilers;
      Html.compilers;
      Html.show_available;
      Html.show_only;
      Html.show_diff_only;
      Html.show_latest_only;
      Html.sort_by_revdeps;
      Html.maintainers;
      Html.logsearch;
    }

  let filter_path path =
    let path = List.filter (fun file -> not (String.is_empty file)) path in
    if not (List.for_all Oca_lib.is_valid_filename path) then
      failwith "Forbidden path";
    path

  let path_from_uri uri =
    match Uri.path uri with
    | "" -> []
    | path -> filter_path (Fpath.segs (Fpath.v path))

  let get_logdir name =
    let cache = Lazy.force Backend.cache in
    let logdirs = Cache.get_logdirs cache in
    List.find_opt (fun logdir ->
      String.equal (Server_workdirs.get_logdir_name logdir) name
    ) logdirs

  let callback ~conf backend _flow reqd =
    let cache = Lazy.force Backend.cache in
    let () = Httpcats_utils.drain_body reqd in
    (* TODO: support fragments and queries *)
    let uri = Uri.make ~path:(Httpcats_utils.target reqd) () in
    let get_log ~logdir ~comp ~state ~pkg =
      match get_logdir logdir with
      | None ->
          Httpcats_utils.respond ~status:`Not_found reqd `Empty
      | Some logdir ->
          let comp = Intf.Compiler.from_string comp in
          let state = Intf.State.from_string state in
          match Backend.get_log backend ~logdir ~comp ~state ~pkg with
          | None ->
              Httpcats_utils.respond ~status:`Not_found reqd `Empty
          | Some log ->
              let html = Html.get_log ~comp ~pkg log in
              Httpcats_utils.respond ~status:`OK reqd (`Html html)
    in
    match path_from_uri uri with
    | [] ->
        begin match Cache.get_latest_logdir cache with
        | None ->
            Httpcats_utils.respond ~status:`OK reqd
              (`Plain_text
                 "opam-health-check: no run exist, please wait for the first run \
                  to finish. Please look at the documentation to learn how to \
                  start it.\n")
        | Some logdir ->
            let query = parse_raw_query logdir uri in
            let html = Cache.get_html ~conf cache query logdir in
            Httpcats_utils.respond ~status:`OK reqd (`Html html)
        end
    | ["run"] ->
        let html = Cache.get_html_run_list cache in
        Httpcats_utils.respond ~status:`OK reqd (`Html html)
    | ["run";logdir] ->
        begin match get_logdir logdir with
        | None ->
            Httpcats_utils.respond ~status:`Not_found reqd `Empty
        | Some logdir ->
            let query = parse_raw_query logdir uri in
            let html = Cache.get_html ~conf cache query logdir in
            Httpcats_utils.respond ~status:`OK reqd (`Html html)
        end
    | ["diff"] ->
        let html = Cache.get_html_diff_list cache in
        Httpcats_utils.respond ~status:`OK reqd (`Html html)
    | ["diff"; range] ->
        let (old_logdir, new_logdir) = match String.split_on_char '.' range with
          | [old_logdir; ""; new_logdir] -> (old_logdir, new_logdir)
          | _ -> assert false
        in
        begin match get_logdir old_logdir with
        | None ->
            Httpcats_utils.respond ~status:`Not_found reqd `Empty
        | Some old_logdir ->
            match get_logdir new_logdir with
            | None ->
                Httpcats_utils.respond ~status:`Not_found reqd `Empty
            | Some new_logdir ->
                let html = Cache.get_html_diff ~conf ~old_logdir ~new_logdir cache in
                Httpcats_utils.respond ~status:`OK reqd (`Html html)
        end
    | ["log"; logdir; comp; state; pkg] ->
        get_log ~logdir ~comp ~state ~pkg
    | ["api"; "v1"; "latest"; "packages"] ->
        let json = Cache.get_json_latest_packages cache in
        Httpcats_utils.respond ~status:`OK reqd (`Json json)
    | _ ->
        Httpcats_utils.respond ~status:`Not_found reqd `Empty

  let tcp_server port handler =
    Miou.async @@ fun () ->
    Httpcats.Server.clear ~handler (Unix.ADDR_INET (Unix.inet_addr_loopback, port))

  let main ~debug ~workdir =
    Printexc.record_backtrace debug;
    let cwd = Unix.getcwd () in
    let workdir = Server_workdirs.create ~cwd ~workdir in
    let () = Server_workdirs.init_base workdir in
    let conf = Server_configfile.from_workdir workdir in
    let port = Server_configfile.port conf in
    let (backend, backend_task, finalizer) = Backend.start ~debug conf workdir in
    List.iter (function Ok () -> () | Error e -> raise e) @@
    let res =
      Miou.await_all [
        tcp_server port (callback ~conf backend);
        backend_task ();
      ]
    in
    finalizer ();
    res
end
