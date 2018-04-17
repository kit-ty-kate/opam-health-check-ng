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
    | ".."::_ -> failwith "You bastard !"
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

let serv_file ~content_type ~logdir file =
  let headers = Cohttp.Header.init_with "Content-Type" content_type in
  let fname = Filename.concat logdir file in
  Cohttp_lwt_unix.Server.respond_file ~headers ~fname ()

let callback ~logdir ~compilers ~pkgs _conn req _body =
  match Path.of_uri (Cohttp.Request.uri req) with
  | [] ->
      serv_string ~content_type:"text/html" (Diff.get_html compilers pkgs)
  | "diff"::compilers ->
      let compilers = List.map Diff.comp_from_string compilers in
      serv_string ~content_type:"text/html" (Diff.get_html compilers pkgs)
  | path ->
      serv_file ~content_type:"text/plain" ~logdir (Path.to_string path)

let () =
  match Sys.argv with
  | [|_; logdir; port|] ->
      begin match Diff.get_dirs logdir with
      | [] ->
          prerr_endline "Empty logdir";
          exit 1
      | compilers ->
          let pkgs = Diff.get_pkgs ~logdir compilers in
          let callback = callback ~logdir ~compilers ~pkgs in
          let port = int_of_string port in
          Lwt_main.run begin
            Cohttp_lwt_unix.Server.create
              ~on_exn:(fun _ -> ())
              ~mode:(`TCP (`Port port))
              (Cohttp_lwt_unix.Server.make ~callback ())
          end
      end
  | _ ->
      prerr_endline "Read the code and try again";
      exit 1
