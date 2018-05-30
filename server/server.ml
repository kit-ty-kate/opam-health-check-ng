open Containers
open Lwt.Infix

module Path : sig
  type t = string list

  val of_uri : Uri.t -> t
  val to_string : t -> string
end = struct
  type elt = string
  type t = elt list

  let rec normalize_path = function
    | [] -> []
    | ""::xs -> normalize_path xs
    | x::_ when String.equal x Filename.parent_dir_name -> failwith "You bastard !"
    | x::_ when String.mem ~sub:Filename.dir_sep x -> failwith "You bastard !!"
    | x::xs -> x :: normalize_path xs

  let of_uri path =
    normalize_path (String.split_on_char '/' (Uri.path path))

  let to_string path =
    String.concat Filename.dir_sep path
end

let serv_string ~content_type body =
  let headers = Cohttp.Header.init_with "Content-Type" content_type in
  Cohttp_lwt_unix.Server.respond_string ~headers ~status:`OK ~body ()

let serv_file ~content_type workdir file =
  let headers = Cohttp.Header.init_with "Content-Type" content_type in
  let fname = Server_workdirs.file_from_logdir ~file workdir in
  Cohttp_lwt_unix.Server.respond_file ~headers ~fname ()

let parse_raw_query workdir raw_query =
  let (variables, compilers) =
    let aux (variables, compilers) str =
      match String.Split.left ~by:":" str with
      | None -> (variables, str::compilers)
      | Some var -> (var::variables, compilers)
    in
    List.fold_left aux ([], []) raw_query
  in
  let (show_available, variables) =
    List.partition (fun (x, _) -> String.equal x "show-available") variables
  in
  if not (List.is_empty variables) then
    failwith "Option unrecognized";
  let logdir = Server_workdirs.logdir workdir in
  begin match compilers with
  | [] -> Diff.get_dirs logdir
  | compilers -> Lwt.return (List.map Diff.comp_from_string compilers)
  end >>= fun compilers ->
  let show_available = match show_available with
    | [] ->
        compilers
    | [(_, show_available)] ->
        List.map Diff.comp_from_string (String.split_on_char ':' show_available)
    | _ ->
        failwith "variable show-available already set"
  in
  Lwt.return {
    Diff.compilers;
    Diff.show_available;
  }

let callback workdir _conn req _body =
  match Path.of_uri (Cohttp.Request.uri req) with
  | [] as raw_query | "diff"::raw_query ->
      parse_raw_query workdir raw_query >>= fun query ->
      Cache.get_html workdir query >>= fun html ->
      serv_string ~content_type:"text/html" html
  | path ->
      serv_file ~content_type:"text/plain" workdir (Path.to_string path)

let tcp_server port callback =
  Cohttp_lwt_unix.Server.create
    ~on_exn:(fun _ -> ())
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback ())

let main workdir =
  Lwt_main.run begin
    Nocrypto_entropy_lwt.initialize () >>= fun () ->
    let workdir = Server_workdirs.create workdir in
    Server_workdirs.init_base workdir >>= fun () ->
    let conf = Server_configfile.from_workdir workdir in
    let port = Server_configfile.port conf in
    let admin_port = Server_configfile.admin_port conf in
    let callback = callback workdir in
    let admin_callback = Admin.callback workdir in
    Admin.create_admin_key workdir >>= fun () ->
    Lwt.join [
      tcp_server port callback;
      tcp_server admin_port admin_callback;
    ]
  end

let term =
  let ($) = Cmdliner.Term.($) in
  Cmdliner.Term.pure main $
  Cmdliner.Arg.(required & pos 0 (some string) None & info ~docv:"WORKDIR" [])

let info =
  Cmdliner.Term.info
    ~doc:"A server to check for broken opam packages."
    ~man:[`P "This program takes a work directory where every files created \
              are stored. This includes logs, config file and user private \
              keys."]
    ~version:Config.version
    Config.name

let () = Cmdliner.Term.exit (Cmdliner.Term.eval (term, info))
