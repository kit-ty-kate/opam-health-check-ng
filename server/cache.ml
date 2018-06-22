open Lwt.Infix

open Backend.Intf

module Html_cache = Hashtbl.Make (struct
    type t = Diff.query
    let hash = Hashtbl.hash
    let equal {Diff.available_compilers; compilers; show_available; show_failures_only; show_diff_only; show_latest_only; maintainers} y =
      List.equal Compiler.equal available_compilers y.Diff.available_compilers &&
      List.equal Compiler.equal compilers y.Diff.compilers &&
      List.equal Compiler.equal show_available y.Diff.show_available &&
      Bool.equal show_failures_only y.Diff.show_failures_only &&
      Bool.equal show_diff_only y.Diff.show_diff_only &&
      Bool.equal show_latest_only y.Diff.show_latest_only &&
      String.equal (fst maintainers) (fst y.Diff.maintainers)
  end)

let pkgsinfo = ref Lwt.return_nil
let html_tbl = Html_cache.create 32
let pkgs = ref Lwt.return_nil
let compilers = ref Lwt.return_nil

let clear_and_init backend =
  pkgsinfo := Metainfo.get_pkgsinfo ();
  compilers := Backend.get_compilers backend;
  pkgs := Backend.get_pkgs !pkgsinfo !compilers backend;
  Html_cache.clear html_tbl

let get_html query =
  !pkgs >>= fun pkgs ->
  let html = Diff.get_html query pkgs in
  Html_cache.add html_tbl query html;
  Lwt.return html

let get_html query =
  match Html_cache.find_opt html_tbl query with
  | Some html -> Lwt.return html
  | None -> get_html query

let get_compilers () =
  !compilers
