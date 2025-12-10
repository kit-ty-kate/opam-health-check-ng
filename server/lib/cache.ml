open Intf

module Opams_cache = Map.Make (String)
module Revdeps_cache = Map.Make (String)

type merge =
  | Old
  | New

module Pkg_htbl = Map.Make (struct
    type t = string * Compiler.t
    let compare (full_name, comp) y =
      let full_name = String.compare full_name (fst y) in
      if full_name <> 0 then full_name
      else Intf.Compiler.compare comp (snd y)
  end)

let add_diff htbl acc ((full_name, comp) as pkg) =
  match Pkg_htbl.find pkg htbl with
  | [((Old | New), Intf.State.NotAvailable)] -> acc
  | [(Old, state)] -> Intf.Pkg_diff.{full_name; comp; diff = NotAvailableAnymore state} :: acc
  | [(New, state)] -> Intf.Pkg_diff.{full_name; comp; diff = NowInstallable state} :: acc
  | [(New, new_state); (Old, old_state)] when Intf.State.equal new_state old_state -> acc
  | [(New, new_state); (Old, old_state)] -> Intf.Pkg_diff.{full_name; comp; diff = StatusChanged (old_state, new_state)} :: acc
  | _ -> assert false
  [@@ocaml.warning "-fragile-match"]

let split_diff (bad, partial, not_available, internal_failure, good) diff =
  let open Intf.State in
  let open Intf.Pkg_diff in
  match diff with
  | {diff = (StatusChanged (_, Bad) | NowInstallable Bad); _} -> (diff :: bad, partial, not_available, internal_failure, good)
  | {diff = (StatusChanged (_, Partial) | NowInstallable Partial); _} -> (bad, diff :: partial, not_available, internal_failure, good)
  | {diff = (StatusChanged (_, NotAvailable) | NotAvailableAnymore _); _} -> (bad, partial, diff :: not_available, internal_failure, good)
  | {diff = (StatusChanged (_, InternalFailure) | NowInstallable InternalFailure); _} -> (bad, partial, not_available, diff :: internal_failure, good)
  | {diff = (StatusChanged (_, Good) | NowInstallable Good); _} -> (bad, partial, not_available, internal_failure, diff :: good)
  | {diff = NowInstallable NotAvailable; _} -> assert false

let generate_diff old_pkgs new_pkgs =
  let pkg_htbl = Pkg_htbl.empty in
  let aux pos pkg_htbl pkg =
    Intf.Pkg.instances pkg |>
    List.fold_left begin fun pkg_htbl inst ->
      let comp = Intf.Instance.compiler inst in
      let state = Intf.Instance.state inst in
      let key = (Intf.Pkg.full_name pkg, comp) in
      match Pkg_htbl.find_opt key pkg_htbl with
      | Some acc -> Pkg_htbl.add key ((pos, state) :: acc) pkg_htbl
      | None -> Pkg_htbl.add key [(pos, state)] pkg_htbl
    end pkg_htbl
  in
  let pkg_htbl = List.fold_left (aux Old) pkg_htbl old_pkgs in
  let pkg_htbl = List.fold_left (aux New) pkg_htbl new_pkgs in
  List.sort_uniq ~cmp:Ord.(pair string Intf.Compiler.compare) (List.map fst (Pkg_htbl.bindings pkg_htbl)) |>
  List.fold_left (add_diff pkg_htbl) [] |>
  List.fold_left split_diff ([], [], [], [], [])

type 'a prefetched_or_recompute =
  | Prefetched of 'a
  | Recompute of (unit -> 'a)

type data = {
  mutable logdirs : Server_workdirs.logdir list Miou.t;
  mutable pkgs : (Server_workdirs.logdir * Intf.Pkg.t list prefetched_or_recompute) list Miou.t;
  mutable compilers : (Server_workdirs.logdir * Intf.Compiler.t list) list Miou.t;
  mutable opams : OpamFile.OPAM.t Opams_cache.t Miou.t;
  mutable revdeps : int Revdeps_cache.t Miou.t;
}

type t = data Miou.t ref

let create_data () = {
  logdirs = Miou.async (fun () -> []);
  pkgs = Miou.async (fun () -> []);
  compilers = Miou.async (fun () -> []);
  opams = Miou.async (fun () -> Opams_cache.empty);
  revdeps = Miou.async (fun () -> Revdeps_cache.empty);
}

let create () = ref (Miou.async (fun () -> create_data ()))

let destroy r_self =
  Miou.cancel !r_self

let clear_and_init r_self ~pkgs ~compilers ~logdirs ~opams ~revdeps =
  let timer = Oca_lib.timer_start () in
  let self = create_data () in
  let trigger = Miou_sync.Trigger.create () in
  Miou.cancel !r_self;
  r_self := Miou.async (fun () ->
    Option.iter (fun (exn, bt) -> Printexc.raise_with_backtrace exn bt) (Miou_sync.Trigger.await trigger);
    self
  );
  Miou.cancel self.opams;
  self.opams <- Miou.async opams;
  Miou.cancel self.revdeps;
  self.revdeps <- Miou.async revdeps;
  Miou.cancel self.logdirs;
  self.logdirs <- Miou.async logdirs;
  Miou.cancel self.compilers;
  self.compilers <- Miou.async (fun () ->
    let logdirs = Miou.await_exn self.logdirs in
    List.map (fun logdir ->
      let c = compilers logdir in
      (logdir, c)
    ) logdirs
  );
  Miou.cancel self.pkgs;
  self.pkgs <- Miou.async (fun () ->
    let compilers = Miou.await_exn self.compilers in
    List.mapi (fun i (logdir, compilers) ->
      let p =
        let aux () = pkgs ~compilers logdir in
        match i with
        | 0 | 1 ->
            let p = aux () in
            (Prefetched p)
        | _ ->
            (Recompute aux)
      in
      (logdir, p)
    ) compilers
  );
  let _ = Miou.await_exn self.opams in
  let _ = Miou.await_exn self.revdeps in
  let _ = Miou.await_exn self.logdirs in
  let _ = Miou.await_exn self.compilers in
  Miou_sync.Trigger.signal trigger;
  let _ = Miou.await_exn self.pkgs in
  Oca_lib.timer_log timer (Miou_unix.of_file_descr Unix.stderr) "Cache prefetching"

let is_deprecated flag =
  String.equal (OpamTypesBase.string_of_pkg_flag flag) "deprecated"

let must_show_package ~logsearch query ~is_latest pkg =
  let opam = Pkg.opam pkg in
  let instances' = Pkg.instances pkg in
  let instances = List.filter (fun inst -> List.mem ~eq:Compiler.equal (Instance.compiler inst) query.Html.compilers) instances' in
  begin
    List.exists (fun comp ->
      match List.find_opt (fun inst -> Compiler.equal comp (Instance.compiler inst)) instances' with
      | None -> true (* TODO: Maybe switch to assert false? *)
      | Some inst -> match Instance.state inst with
        | State.NotAvailable -> false
        | State.(Good | Partial | Bad | InternalFailure) -> true
    ) query.Html.show_available
  end && begin
    List.exists (fun state ->
      List.exists (fun inst -> State.equal state (Instance.state inst)) instances
    ) query.Html.show_only
  end && begin
    match instances with
    | hd::tl when query.Html.show_diff_only ->
        let state = Instance.state hd in
        List.exists (fun x -> not (State.equal state (Instance.state x))) tl
    | [] | _::_ ->
        true
  end && begin
    if query.Html.show_latest_only then
      if is_latest then
        not (List.exists is_deprecated opam.OpamFile.OPAM.flags)
      else
        false
    else
      true
  end && begin
    match snd query.Html.maintainers with
    | Some re -> List.exists (Re.execp re) opam.OpamFile.OPAM.maintainer
    | None -> true
  end && begin
    match snd query.Html.logsearch with
    | Some _ ->
        let logsearch = Miou.await_exn logsearch in
        (List.exists (Pkg.equal pkg) logsearch)
    | None -> true
  end

let filter_pkg ~logsearch query (acc, last) pkg =
  let is_latest = match last with
    | None -> true
    | Some last -> not (String.equal (Pkg.name pkg) (Pkg.name last))
  in
  match must_show_package ~logsearch query ~is_latest pkg with
  | true -> (pkg :: acc, Some pkg)
  | false -> (acc, Some pkg)

(* TODO: Make use of the cache *)
let get_logsearch ~query ~logdir =
  Miou.async @@ fun () ->
  match query.Html.logsearch with
  | _, None -> []
  | regexp, Some (_, comp) ->
      let switch = Compiler.to_string comp in
      let searches = Server_workdirs.logdir_search ~switch ~regexp logdir in
      List.filter_map (fun s ->
        match String.split_on_char '/' s with
        | [_switch; _state; full_name] -> Some (Pkg.create ~full_name ~instances:[] ~opam:OpamFile.OPAM.empty ~revdeps:(-1))
        | _ -> None
      ) searches

let revdeps_cmp p1 p2 =
  Int.neg (Int.compare (Intf.Pkg.revdeps p1) (Intf.Pkg.revdeps p2))

let get_or_recompute = function
  | Prefetched p -> p
  | Recompute f -> f ()

let get_html ~conf self query logdir =
  let aux ~logdir pkgs =
    let logsearch = get_logsearch ~query ~logdir in
    let (pkgs, _) = List.fold_left (filter_pkg ~logsearch query) ([], None) (List.rev pkgs) in
    let pkgs = if query.Html.sort_by_revdeps then List.sort revdeps_cmp pkgs else pkgs in
    let html = Html.get_html ~logdir ~conf query pkgs in
    Miou.cancel logsearch;
    html
  in
  let pkgs = Miou.await_exn self.pkgs in
  let pkgs = List.assoc ~eq:Server_workdirs.logdir_equal logdir pkgs in
  aux ~logdir (get_or_recompute pkgs)

let get_latest_logdir self =
  let self = Miou.await_exn !self in
  match Miou.await_exn self.logdirs with
  | [] -> None
  | logdir::_ -> (Some logdir)

let get_html ~conf self query logdir =
  let self = Miou.await_exn !self in
  get_html ~conf self query logdir

let get_logdirs self =
  let self = Miou.await_exn !self in
  Miou.await_exn self.logdirs

let get_pkgs ~logdir self =
  let self = Miou.await_exn !self in
  let pkgs = Miou.await_exn self.pkgs in
  get_or_recompute (List.assoc ~eq:Server_workdirs.logdir_equal logdir pkgs)

let get_compilers ~logdir self =
  let self = Miou.await_exn !self in
  let compilers = Miou.await_exn self.compilers in
  List.assoc ~eq:Server_workdirs.logdir_equal logdir compilers

let get_opam self k =
  let self = Miou.await_exn !self in
  let opams = Miou.await_exn self.opams in
  Option.get_or ~default:OpamFile.OPAM.empty (Opams_cache.find_opt k opams)

let get_revdeps self k =
  let self = Miou.await_exn !self in
  let revdeps = Miou.await_exn self.revdeps in
  Option.get_or ~default:(-1) (Revdeps_cache.find_opt k revdeps)

let get_html_diff ~conf ~old_logdir ~new_logdir self =
  let old_pkgs = get_pkgs ~logdir:old_logdir self in
  let new_pkgs = get_pkgs ~logdir:new_logdir self in
  generate_diff old_pkgs new_pkgs |>
  Html.get_diff ~conf ~old_logdir ~new_logdir

let get_html_diff_list self =
  let self = Miou.await_exn !self in
  let pkgs = Miou.await_exn self.pkgs in
  Oca_lib.list_map_cube (fun (new_logdir, _) (old_logdir, _) -> (old_logdir, new_logdir)) pkgs |>
  Html.get_diff_list

let get_html_run_list self =
  let self = Miou.await_exn !self in
  let pkgs = Miou.await_exn self.pkgs in
  Html.get_run_list (List.map fst pkgs)

let get_json_latest_packages self =
  let self = Miou.await_exn !self in
  let json =
    let pkgs = match Miou.await_exn self.logdirs with
      | [] -> []
      | logdir::_ ->
          let pkgs = Miou.await_exn self.pkgs in
          get_or_recompute (List.assoc ~eq:Server_workdirs.logdir_equal logdir pkgs)
    in
    let json = Json.pkgs_to_json pkgs in
    Yojson.Safe.to_string json
  in
  json
