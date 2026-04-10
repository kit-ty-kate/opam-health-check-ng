let instance_to_json ~logdir ~pkgname instance =
  let compiler = Intf.Compiler.to_string (Intf.Instance.compiler instance) in
  let status = Intf.State.to_string (Intf.Instance.state instance) in
  `Assoc (
    [
      ("compiler", `String compiler);
      ("status", `String status);
    ] @
    match logdir with
    | None -> []
    | Some logdir ->
        let logdir = Server_workdirs.get_logdir_name logdir in
        (* NOTE: sync up with server/lib/server.ml *)
        [("log", `String ("/log/"^logdir^"/"^compiler^"/"^status^"/"^pkgname))]
  )

let pkg_to_json ~logdir pkg =
  let pkgname = Intf.Pkg.full_name pkg in
  let instances = Intf.Pkg.instances pkg in
  `Assoc [
    ("name", `String pkgname);
    ("statuses", `List (List.map (instance_to_json ~logdir ~pkgname) instances));
  ]

let pkgs_to_json ~logdir pkgs =
  `List (List.map (pkg_to_json ~logdir) pkgs)
