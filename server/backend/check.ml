let fmt = Printf.sprintf

(* UID/GID of the user "opam" in the ocaml/opam images *)
let uid = 1000
let gid = 1000

let () = Random.self_init ()

module Dockerfile = struct
  include Dockerfile

  let run ?(network=`None) fmt =
    Printf.ksprintf (fun cmd -> run ~network "<<EOT\n%s\nEOT" cmd) fmt
end

let volumes ~conf =
  (* TODO: remove this hack *)
  let opam_packages_init = {|set -e
mkdir /tmp/tmp-opam-repo
cd /tmp/tmp-opam-repo
repos=$(opam repo list -s)
for pkg in $(cat ~/opam-packages.list) ; do
  path="packages/$(echo "$pkg" | cut -d. -f1)/$pkg"
  for repo in $(echo "$repos") ; do
    opamfile="/home/opam/.opam/repo/$repo/$path/opam"
    if test -f "$opamfile" ; then
      mkdir -p "$path"
      cat "$opamfile" > "$path/opam"
      break
    fi
  done
done
echo 'opam-version: "2.0"' > repo
sudo chown opam:opam ~/opam-cache
opam admin cache ~/opam-cache|}
  in
  (* TODO: Support different distributions *)
  let apt_packages_init = {|set -e
for pkg in $(cat ~/apt-packages.list) ; do
  sudo apt-get install -yy --download-only "$pkg" || true
done|}
  in
  let dune_cache_init = "sudo chown opam:opam /home/opam/.cache/dune" in
  ("opam-archives-cache", "/home/opam/opam-cache", opam_packages_init) ::
  ("opam-apt-cache", "/var/cache/apt/archives", apt_packages_init) ::
  if Server_configfile.enable_dune_cache conf
  then [("opam-dune-cache", "/home/opam/.cache/dune", dune_cache_init)]
  else []

let network = `Default

module Docker_img_hashtbl = Hashtbl.Make (String)
let docker_img_hashtbl = Docker_img_hashtbl.create 1

let docker_build ~conf ~base_dockerfile ~stdout cmd =
  let base_dockerfile = Format.sprintf "%a" Dockerfile.pp base_dockerfile in
  let%lwt tag, volumes =
    match Docker_img_hashtbl.find_opt docker_img_hashtbl base_dockerfile with
    | None ->
        let dockerfile_hash = Hashtbl.hash (base_dockerfile : string) in (* TODO: replace with String.hash when we switch to OCaml >= 5.0 *)
        let tag = fmt "opam-health-check-%s-%d" (Server_configfile.name conf) dockerfile_hash in
        let stdin, fd = Lwt_unix.pipe () in
        let stdin = `FD_move (Lwt_unix.unix_file_descr stdin) in
        Lwt_unix.set_close_on_exec fd;
        let proc = Oca_lib.exec ~stdin ~stdout ~stderr:stdout ["docker";"buildx";"build";"--progress=plain";"-t";tag;"-"] in
        let%lwt () = Oca_lib.write_line fd base_dockerfile in
        let%lwt () = Lwt_unix.close fd in
        begin match%lwt proc with
        | Ok () ->
            let%lwt volumes =
              Lwt_list.fold_left_s (fun acc (volume, path, init) ->
                let volume = ["--volume";volume^":"^path] in
                match%lwt Oca_lib.exec ~stdin:`Close ~stdout ~stderr:stdout (["docker";"run";"--rm"]@volume@[tag;"sh";"-c";init]) with
                | Ok () -> Lwt.return (acc @ volume)
                | Error () -> failwith "volume creation failed"
              ) [] (volumes ~conf)
            in
            Docker_img_hashtbl.add docker_img_hashtbl base_dockerfile tag;
            Lwt.return (tag, volumes)
        | Error () -> failwith "docker build failed"
        end
    | Some tag ->
        let volumes =
          List.fold_left (fun acc (volume, path, _init) ->
            acc @ ["--volume";volume^":"^path]
          ) [] (volumes ~conf)
        in
        Lwt.return (tag, volumes)
  in
  let stdin, fd = Lwt_unix.pipe () in
  let stdin = `FD_move (Lwt_unix.unix_file_descr stdin) in
  Lwt_unix.set_close_on_exec fd;
  let proc = Oca_lib.exec ~stdin ~stdout ~stderr:stdout (["docker";"run";"--rm";"--network=none"]@volumes@["-i";tag]) in
  let%lwt () = Oca_lib.write_line fd cmd in
  let%lwt () = Lwt_unix.close fd in
  proc

let exec_out ~fexec ~fout =
  let stdin, stdout = Lwt_unix.pipe () in
  let proc = (fexec ~stdout) [%lwt.finally Lwt_unix.close stdout] in
  let%lwt res = fout ~stdin in
  let%lwt () = Lwt_unix.close stdin in
  let%lwt r = proc in
  Lwt.return (r, res)

let docker_build_str ~debug ~conf ~base_dockerfile ~stderr ~default c =
  let rec aux ~stdin =
    (* TODO: Use --progress=rawjson whenever it's available *)
    match%lwt Oca_lib.read_line_opt stdin with
    | Some "@@@OUTPUT" ->
        let rec aux acc =
          match%lwt Oca_lib.read_line_opt stdin with
          | Some "@@@OUTPUT" -> Lwt.return (List.rev acc)
          | Some x -> aux (x :: acc)
          | None -> Lwt.fail (Failure "Error: Closing @@@OUTPUT could not be detected")
        in
        aux []
    | Some line ->
        let%lwt () = (if debug then Oca_lib.write_line stderr line else Lwt.return_unit) in
        aux ~stdin
    | None -> Lwt.return_nil
  in
  match%lwt
    exec_out ~fout:aux ~fexec:(fun ~stdout ->
      docker_build ~conf ~base_dockerfile ~stdout (fmt "echo '%f' > /dev/null && echo @@@OUTPUT && %s && echo @@@OUTPUT" (Unix.time ()) c)
    )
  with
  | (Ok (), r) ->
      Lwt.return r
  | (Error (), _) ->
      match default with
      | None -> Lwt.fail (Failure ("Failure in ocluster: "^c)) (* TODO: Replace this with "send message to debug slack webhook" *)
      | Some v -> Lwt.return v

let failure_kind conf ~pkg logfile =
  let pkgname, pkgversion = match Astring.String.cut ~sep:"." pkg with
    | Some x -> x
    | None -> Fmt.failwith "Package %S could not be parsed" pkg
  in
  let timeout = Server_configfile.job_timeout conf in
  Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string logfile) begin fun ic ->
    let rec lookup res =
      match%lwt Lwt_io.read_line_opt ic with
      | Some "+- The following actions failed" -> lookup `Failure
      | Some "+- The following actions were aborted" -> Lwt.return `Partial
      | Some line when String.equal ("[ERROR] No package named "^pkgname^" found.") line ||
                       String.equal ("[ERROR] Package "^pkgname^" has no version "^pkgversion^".") line ->
          lookup `NotAvailable
      | Some "[ERROR] Package conflict!" -> lookup `NotAvailable
      | Some "This package failed and has been disabled for CI using the 'x-ci-accept-failures' field." -> lookup `AcceptFailures
      | Some line when String.equal ("+++ Timeout!! ("^string_of_float timeout^" hours) +++") line -> Lwt.return `Timeout
      | Some line when String.prefix ~pre:"#=== ERROR while fetching sources for " line -> Lwt.return `Other
      | Some _ -> lookup res
      | None -> Lwt.return res
    in
    lookup `Other
  end

let with_test pkg = fmt {|
test $res != 0 && exit $res
opam reinstall -j1 -vty '%s'
res=$?
test $res = 20 && res=0
|} pkg

let with_test ~conf pkg =
  if Server_configfile.with_test conf then
    with_test pkg
  else
    ""

let with_lower_bound pkg = fmt {|
test $res != 0 && exit $res
env 'OPAMCRITERIA=+removed,+count[version-lag,solution]' opam reinstall -j1 -vy '%s'
res=$?
|} pkg

let with_lower_bound ~conf pkg =
  if Server_configfile.with_lower_bound conf then
    with_lower_bound pkg
  else
    ""

let run_script ~conf pkg = fmt {|
opam install -j1 -vy '%s'
res=$?
if test $res = 31 ; then
  build_dir=$(opam var prefix)/.opam-switch/build
  failed=$(ls "$build_dir")
  partial=""
  for pkg in $failed ; do
    if opam show -f x-ci-accept-failures: "$pkg" | grep -qF '"%s"' ; then
      echo "A package failed and has been disabled for CI using the 'x-ci-accept-failures' field."
    fi
    test "$pkg" != '%s' && partial="$partial $pkg"
  done
  test "$partial" != "" && echo "opam-health-check detected dependencies failing: ${partial_fails}"
fi
%s
%s
exit $res
|} pkg pkg (Server_configfile.platform_distribution conf) (with_test ~conf pkg) (with_lower_bound ~conf pkg)

let run_job ~conf ~pool ~stderr ~base_dockerfile ~switch ~num logdir pkg =
  Lwt_pool.use pool begin fun () ->
    let%lwt () = Oca_lib.write_line stderr ("["^num^"] Checking "^pkg^" on "^Intf.Switch.switch switch^"…") in
    let switch = Intf.Switch.name switch in
    let logfile = Server_workdirs.tmplogfile ~pkg ~switch logdir in
    match%lwt
      Oca_lib.with_file Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o640 (Fpath.to_string logfile) (fun stdout ->
        docker_build ~conf ~base_dockerfile ~stdout (run_script ~conf pkg)
      )
    with
    | Ok () ->
        let%lwt () = Oca_lib.write_line stderr ("["^num^"] succeeded.") in
        Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpgoodlog ~pkg ~switch logdir))
    | Error () ->
        begin match%lwt failure_kind conf ~pkg logfile with
        | `Partial ->
            let%lwt () = Oca_lib.write_line stderr ("["^num^"] finished with a partial failure.") in
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmppartiallog ~pkg ~switch logdir))
        | `Failure ->
            let%lwt () = Oca_lib.write_line stderr ("["^num^"] failed.") in
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpbadlog ~pkg ~switch logdir))
        | `NotAvailable ->
            let%lwt () = Oca_lib.write_line stderr ("["^num^"] finished with not available.") in
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpnotavailablelog ~pkg ~switch logdir))
        | `Other | `AcceptFailures | `Timeout ->
            let%lwt () = Oca_lib.write_line stderr ("["^num^"] finished with an internal failure.") in
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpinternalfailurelog ~pkg ~switch logdir))
        end
  end

let () =
  Lwt.async_exception_hook := begin fun e ->
    prerr_endline ("Async exception raised: "^Printexc.to_string e);
    prerr_string (Printexc.get_backtrace ());
    flush stderr;
  end

let get_dockerfile ~conf ~opam_repo ~opam_repo_commit ~extra_repos switch =
  let extra_repos =
    let switch = Intf.Switch.name switch in
    List.filter (fun (repo, _) ->
      match Intf.Repository.for_switches repo with
      | None -> true
      | Some for_switches -> List.exists (Intf.Compiler.equal switch) for_switches
    ) extra_repos
  in
  let open! Dockerfile in
  let os = Server_configfile.platform_os conf in
  let distribution = Server_configfile.platform_distribution conf in
  let img = match os with
    | "linux" -> "ocaml/opam:"^distribution (* typically this is 'debian-unstable' which is 5.0.0 *)
    | "freebsd" -> distribution
    | "macos" -> "macos-"^distribution
    | os -> failwith ("OS '"^os^"' not supported") (* TODO: Should other platforms simply take the same ocurrent/opam: prefix? *)
  in
  let ln_opam = match os with
    | "linux" -> "sudo ln -f /usr/bin/opam-dev /usr/bin/opam"
    | "freebsd" -> "sudo ln -f /usr/local/bin/opam-dev /usr/local/bin/opam"
    | "macos" -> "ln -f ~/local/bin/opam-dev ~/local/bin/opam"
    | os -> failwith ("OS '"^os^"' not supported")
  in
  from img @@
  user "%d:%d" uid gid @@
  env [
    "OPAMPRECISETRACKING", "1"; (* NOTE: See https://github.com/ocaml/opam/issues/3997 *)
    "OPAMUTF8", "never"; (* Disable UTF-8 characters so that output stay consistant accross platforms *)
    "OPAMEXTERNALSOLVER", "builtin-0install";
    "OPAMCRITERIA", "+removed";
  ] @@
  run "%s" ln_opam @@
  run ~network "rm -rf ~/opam-repository && git clone -q '%s' ~/opam-repository && git -C ~/opam-repository checkout -q %s" (Intf.Github.url opam_repo) opam_repo_commit @@
  run "rm -rf ~/.opam && opam init -ya --bare ~/opam-repository" @@@
  List.flatten (
    List.map (fun (repo, hash) ->
      let name = Filename.quote (Intf.Repository.name repo) in
      let url = Intf.Github.url (Intf.Repository.github repo) in
      [ run ~network "git clone -q '%s' ~/%s && git -C ~/%s checkout -q %s" url name name hash;
        run "opam repository add --dont-select %s ~/%s" name name;
      ]
    ) extra_repos
  ) @ [
    run ~network "opam switch create --repositories=%sdefault '%s' '%s'"
      (List.fold_left (fun acc (repo, _) -> Intf.Repository.name repo^","^acc) "" extra_repos)
      (Intf.Compiler.to_string (Intf.Switch.name switch))
      (Intf.Switch.switch switch);
    run "opam list --available --installable --short --all-versions > ~/opam-packages.list";
    run "opam list --depexts --available --installable --short --all-versions > ~/apt-packages.list";
    run {|opam option --global 'archive-mirrors=["file:///home/opam/opam-cache"]'|};
    run ~network {|sudo rm /etc/apt/apt.conf.d/docker-clean && (echo 'APT::Install-Recommends "false";' && echo 'Debug::NoLocking "true";') | sudo tee -a /etc/apt/apt.conf && opam update --depexts|};
  ] @
  (* TODO: Should this be removed now that it is part of the base docker images? What about macOS? *)
  (if OpamVersionCompare.compare (Intf.Switch.switch switch) "4.08" < 0 then
     [run ~network "opam install -y ocaml-secondary-compiler"]
     (* NOTE: See https://github.com/ocaml/opam-repository/pull/15404
        and https://github.com/ocaml/opam-repository/pull/15642 *)
   else
     []
  ) @
  (match Server_configfile.extra_command conf with
   | Some c -> [run ~network "%s" c]
   | None -> []
  ) @
  (if Server_configfile.enable_dune_cache conf then
     [ run ~network "opam pin add -k version dune $(opam show -f version dune)";
       env [
         "DUNE_CACHE", "enabled";
         "DUNE_CACHE_TRANSPORT", "direct";
         "DUNE_CACHE_STORAGE_MODE", "copy";
       ]
     ]
   else
     []
  ) @ [
    env ["OCAMLPARAM", "warn-error=+8,_"]; (* https://github.com/ocaml/ocaml/issues/12475 *)
  ]

let get_pkgs ~debug ~conf ~stderr (switch, base_dockerfile) =
  let switch = Intf.Compiler.to_string (Intf.Switch.name switch) in
  let%lwt () = Oca_lib.write_line stderr ("Getting packages list for "^switch^"… (this may take an hour or two)") in
  let%lwt pkgs = docker_build_str ~debug ~conf ~base_dockerfile ~stderr ~default:None (Server_configfile.list_command conf) in
  let pkgs = List.filter begin fun pkg ->
    Oca_lib.is_valid_filename pkg &&
    match Intf.Pkg.name (Intf.Pkg.create ~full_name:pkg ~instances:[] ~opam:OpamFile.OPAM.empty ~revdeps:0) with (* TODO: Remove this horror *)
    | "ocaml" | "ocaml-base-compiler" | "ocaml-variants" | "ocaml-beta" | "ocaml-config" -> false
    | "ocaml-option-32bit"
    | "ocaml-option-afl"
    | "ocaml-option-bytecode-only"
    | "ocaml-option-default-unsafe-string"
    | "ocaml-option-flambda"
    | "ocaml-option-fp"
    | "ocaml-option-musl"
    | "ocaml-option-nnp"
    | "ocaml-option-nnpchecker"
    | "ocaml-option-no-flat-float-array"
    | "ocaml-option-spacetime"
    | "ocaml-option-static"
    | "ocaml-options-only-afl"
    | "ocaml-options-only-flambda"
    | "ocaml-options-only-flambda-fp"
    | "ocaml-options-only-fp"
    | "ocaml-options-only-nnp"
    | "ocaml-options-only-nnpchecker"
    | "ocaml-options-only-no-flat-float-array"
    | "ocaml-options-vanilla" -> false
    | _ -> true
  end pkgs in
  let nelts = string_of_int (List.length pkgs) in
  let%lwt () = Oca_lib.write_line stderr ("Package list for "^switch^" retrieved. "^nelts^" elements to process.") in
  Lwt.return pkgs

let with_stderr ~start_time workdir f =
  let%lwt () = Oca_lib.mkdir_p (Server_workdirs.ilogdir workdir) in
  let logfile = Server_workdirs.new_ilogfile ~start_time workdir in
  Oca_lib.with_file Unix.[O_WRONLY; O_CREAT; O_APPEND] 0o640 (Fpath.to_string logfile) (fun stderr -> f ~stderr)

module Pkg_set = Set.Make (String)

let revdeps_script pkg =
  let pkg = Filename.quote pkg in
  {|opam list --color=never -s --recursive --depopts --depends-on |}^pkg^{| && \
    opam list --color=never -s --with-test --with-doc --depopts --depends-on |}^pkg

let get_metadata ~debug ~conf ~jobs ~pool ~stderr logdir (_, base_dockerfile) pkgs =
  let get_revdeps ~base_dockerfile ~pkgname ~pkg ~logdir =
    let%lwt revdeps = docker_build_str ~debug ~conf ~base_dockerfile ~stderr ~default:(Some []) (revdeps_script pkg) in
    let module Set = Set.Make(String) in
    let revdeps = Set.of_list revdeps in
    let revdeps = Set.remove pkgname revdeps in (* https://github.com/ocaml/opam/issues/4446 *)
    Lwt_io.with_file ~mode:Lwt_io.output (Fpath.to_string (Server_workdirs.tmprevdepsfile ~pkg logdir)) (fun c ->
      Lwt_io.write c (string_of_int (Set.cardinal revdeps))
    )
  in
  let get_latest_metadata ~base_dockerfile ~pkgname ~logdir = (* TODO: Get this locally by merging all the repository and parsing the opam files using opam-core *)
    let%lwt opam =
      docker_build_str ~debug ~conf ~base_dockerfile ~stderr ~default:(Some [])
        ("opam show --raw "^Filename.quote pkgname)
    in
    Lwt_io.with_file ~mode:Lwt_io.output (Fpath.to_string (Server_workdirs.tmpopamfile ~pkg:pkgname logdir)) (fun c ->
      Lwt_io.write c (String.concat "\n" opam)
    )
  in
  Pkg_set.fold begin fun full_name (pkgs_set, jobs) ->
    let pkgname = Intf.Pkg.name (Intf.Pkg.create ~full_name ~instances:[] ~opam:OpamFile.OPAM.empty ~revdeps:0) in (* TODO: Remove this horror *)
    let job =
      Lwt_pool.use pool begin fun () ->
        let%lwt () = Oca_lib.write_line stderr ("Getting metadata for "^full_name) in
        let%lwt () = get_revdeps ~base_dockerfile ~pkgname ~pkg:full_name ~logdir in
        if Pkg_set.mem pkgname pkgs_set then Lwt.return_unit else get_latest_metadata ~base_dockerfile ~pkgname ~logdir
      end
    in
    (Pkg_set.add pkgname pkgs_set, job :: jobs)
  end pkgs (Pkg_set.empty, jobs)

let get_commit_hash github =
  let user = Intf.Github.user github in
  let repo = Intf.Github.repo github in
  let branch = Intf.Github.branch github in
  let%lwt r =
    Github.Monad.run begin
      let ( >>= ) = Github.Monad.( >>= ) in
      Github.Repo.info ~user ~repo () >>= fun info ->
      let branch = match branch with
        | None -> info#value.Github_t.repository_default_branch
        | Some _ -> branch
      in
      let branch = Option.value ~default:"master" branch in
      Github.Repo.get_ref ~user ~repo ~name:("heads/"^branch) ()
    end
  in
  let r = Github.Response.value r in
  Lwt.return (r.Github_t.git_ref_obj.Github_t.obj_sha)

let get_commit_hash_default conf =
  let github = Server_configfile.default_repository conf in
  let%lwt hash = get_commit_hash github in
  Lwt.return (github, hash)

let get_commit_hash_extra_repos conf =
  Lwt_list.map_s begin fun repository ->
    let github = Intf.Repository.github repository in
    let%lwt hash = get_commit_hash github in
    Lwt.return (repository, hash)
  end (Server_configfile.extra_repositories conf)

let move_tmpdirs_to_final ~switches logdir workdir =
  let metadatadir = Server_workdirs.metadatadir workdir in
  let tmpmetadatadir = Server_workdirs.tmpmetadatadir logdir in
  let tmpdir = Server_workdirs.tmpdir logdir in
  let switches = List.map Intf.Switch.name switches in
  let%lwt () = Server_workdirs.logdir_move ~switches logdir in
  let%lwt () = Oca_lib.rm_rf metadatadir in
  let%lwt () = Lwt_unix.rename (Fpath.to_string tmpmetadatadir) (Fpath.to_string metadatadir) in
  Oca_lib.rm_rf tmpdir

let run_jobs ~conf ~pool ~stderr logdir switches pkgs =
  (* NOTE: Randomize the list to avoid having several containers downloading the same archives at the same time *)
  let pkgs = Pkg_set.to_list pkgs in
  let pkgs = List.fast_sort (fun _ _ -> if Random.bool () then 1 else -1) pkgs in
  let len_suffix = "/"^string_of_int (List.length pkgs * List.length switches) in
  List.fold_left begin fun (i, jobs) full_name ->
    List.fold_left begin fun (i, jobs) (switch, base_dockerfile) ->
      let i = succ i in
      let num = string_of_int i^len_suffix in
      let job = run_job ~conf ~pool ~stderr ~base_dockerfile ~switch ~num logdir full_name in
      (i, job :: jobs)
    end (i, jobs) switches
  end (0, []) pkgs

let trigger_slack_webhooks ~stderr ~old_logdir ~new_logdir conf =
  let public_url = Server_configfile.public_url conf in
  let body = match old_logdir with
    | Some old_logdir ->
        let old_logdir = Server_workdirs.get_logdir_name old_logdir in
        let new_logdir = Server_workdirs.get_logdir_name new_logdir in
        Printf.sprintf {|{"username": "opam-health-check", "text":"The latest check is done. Check out %s/diff/%s..%s to discover which packages are now broken or fixed"}|} public_url old_logdir new_logdir
    | None ->
        Printf.sprintf {|{"text":"The first check is done. Check out %s to discover which packages are now broken or fixed"}|} public_url
  in
  Server_configfile.slack_webhooks conf |>
  Lwt_list.iter_s begin fun webhook ->
    let%lwt () = Oca_lib.write_line stderr ("Triggering Slack webhook "^Uri.to_string webhook) in
    match%lwt
      Http_lwt_client.request
        ~config:(`HTTP_1_1 Httpaf.Config.default) (* TODO: Remove this when https://github.com/roburio/http-lwt-client/issues/7 is fixed *)
        ~meth:`POST
        ~headers:[("Content-type", "application/json")]
        ~body
        (Uri.to_string webhook)
        (fun _resp acc x -> Lwt.return (acc ^ x)) ""
    with
    | Ok ({Http_lwt_client.status = `OK; _}, _body) -> Lwt.return_unit
    | Ok (resp, body) ->
        let resp = Format.sprintf "%a" Http_lwt_client.pp_response resp in
        Oca_lib.write_line stderr (fmt "Webhook returned failure: %s\nBody: %s" resp body)
    | Error (`Msg msg) -> Oca_lib.write_line stderr ("Webhook failed with: "^msg)
  end

let run_locked = ref false

let is_running () = !run_locked

let wait_current_run_to_finish =
  let rec loop () =
    if is_running () then
      let%lwt () = Lwt_unix.sleep 1. in
      loop ()
    else
      Lwt.return_unit
  in
  loop

let update_docker_image conf =
  let repo_and_tag ~image =
    match String.split_on_char ':' image with
    | [] -> assert false
    | [image] -> (image, None)
    | image::tag -> (image, Some (String.concat ":" tag))
  in
  let get_latest_image ~image =
    let repo, tag = repo_and_tag ~image in
    let os = Server_configfile.platform_os conf in
    let arch = match Server_configfile.platform_arch conf with
      | "x86_64" -> "amd64"
      | arch -> arch
    in
    match%lwt Docker_hub.fetch_manifests ~repo ~tag with
    | Ok manifests ->
        begin match Docker_hub.digest ~os ~arch manifests with
        | Ok digest -> Lwt.return_some (image^"@"^digest)
        | Error e ->
           let e = match e with
             | `Malformed_json str -> fmt "malformed json %S" str
             | `No_corresponding_arch_found -> "no corresponding arch found"
             | `No_corresponding_os_found -> "no corresponding os found"
           in
           prerr_endline ("Something went wrong while parsing docker digest: "^e);
           Lwt.return_none
        end
    | Error e ->
       let e = match e with
         | `Api_error (response, body) ->
             Format.asprintf "response: %a, body: %a"
               Http_lwt_client.pp_response response
               (Option.pp String.pp) body
         | `Malformed_json str -> fmt "malformed json %S" str
         | `Msg str -> str
       in
       prerr_endline ("Something went wrong while fetching docker manifests: "^e);
       Lwt.return_none
  in
  let image = Server_configfile.platform_image conf in
  match String.split_on_char '@' image with
  | [] -> assert false
  | [image] ->
      begin match%lwt get_latest_image ~image with
      | Some image -> Server_configfile.set_platform_image conf image
      | None -> Lwt.fail (Failure (fmt "Could not get digest for image '%s'" image))
      end
  | [image; _old_digest] ->
      begin match%lwt get_latest_image ~image with
      | Some image -> Server_configfile.set_platform_image conf image
      | None -> prerr_endline "Defaulting to old digest"; Lwt.return_unit
      end
  | _ -> Lwt.fail (Failure (fmt "Image name '%s' is not valid" image))

let run ~debug ~on_finished ~conf cache workdir =
  let switches = Option.get_exn_or "no switches" (Server_configfile.ocaml_switches conf) in
  if !run_locked then
    failwith "operation locked";
  run_locked := true;
  Lwt.async begin fun () -> Lwt.finalize begin fun () ->
    let start_time = Unix.time () in
    with_stderr ~start_time workdir begin fun ~stderr ->
      try%lwt
        let timer = Oca_lib.timer_start () in
        let%lwt () = update_docker_image conf in
        let%lwt (opam_repo, opam_repo_commit) = get_commit_hash_default conf in
        let%lwt extra_repos = get_commit_hash_extra_repos conf in
        (* TODO *)
(*        let%lwt () =
          match%lwt Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["docker";"system";"prune";"-af"] with
          | Ok () -> Lwt.return_unit
          | Error () -> Lwt.fail (Failure "docker prune failed")
          in *)
        let switches' = switches in
        let switches = List.map (fun switch -> (switch, get_dockerfile ~conf ~opam_repo ~opam_repo_commit ~extra_repos switch)) switches in
        begin match switches with
        | switch::_ ->
            let%lwt old_logdir = Oca_server.Cache.get_logdirs cache in
            let compressed = Server_configfile.enable_logs_compression conf in
            let old_logdir = List.head_opt old_logdir in
            let new_logdir = Server_workdirs.new_logdir ~compressed ~hash:opam_repo_commit ~start_time workdir in
            let%lwt () = Server_workdirs.init_base_jobs ~switches:switches' new_logdir in
            let pool = Lwt_pool.create (Server_configfile.processes conf) (fun () -> Lwt.return_unit) in
            let%lwt pkgs = Lwt_list.map_s (get_pkgs ~debug ~stderr ~conf) switches in
            let pkgs = Pkg_set.of_list (List.concat pkgs) in
            let%lwt () = Oca_lib.timer_log timer stderr "Initialization" in
            let (_, jobs) = run_jobs ~conf ~pool ~stderr new_logdir switches pkgs in
            let (_, jobs) = get_metadata ~debug ~conf ~jobs ~pool ~stderr new_logdir switch pkgs in
            let%lwt () = Lwt.join jobs in
            let%lwt () = Oca_lib.timer_log timer stderr "Operation" in
            let%lwt () = Oca_lib.write_line stderr "Finishing up…" in
            let%lwt () = move_tmpdirs_to_final ~switches:switches' new_logdir workdir in
            let%lwt () = on_finished workdir in
            let%lwt () = trigger_slack_webhooks ~stderr ~old_logdir ~new_logdir conf in
            Oca_lib.timer_log timer stderr "Clean up"
        | [] ->
            Oca_lib.write_line stderr "No switches."
        end
      with
      | exc ->
          let%lwt () = Oca_lib.write_line stderr ("Exception: "^Printexc.to_string exc^".") in
          let%lwt () = Oca_lib.write stderr (Printexc.get_backtrace ()) in
          Lwt.return (prerr_endline "The current run failed unexpectedly. Please check the latest log using: opam-health-check log")
    end
  end (fun () -> run_locked := false; Lwt.return_unit) end;
  Lwt.return_unit
