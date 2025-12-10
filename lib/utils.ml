let with_file flags mode filename f =
  let fd = Unix.openfile filename flags mode in
  let fd = try Miou_unix.of_file_descr fd with e -> Unix.close fd; raise e in
  Fun.protect (fun () -> f fd) ~finally:(fun () -> Miou_unix.close fd)

let with_in file f = with_file [Unix.O_RDONLY] 0o640 file f
let with_out file f = with_file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640 file f

let read_all fd =
  let buf_len = 4096 in
  let final_buf = Buffer.create buf_len in
  let buf = Bytes.create buf_len in
  let rec loop () =
    let n = Miou_unix.read fd buf in
    if n = 0 then
      Buffer.contents final_buf
    else if Int.equal n buf_len then begin
      Buffer.add_bytes final_buf buf;
      loop ()
    end else begin
      Buffer.add_subbytes final_buf buf 0 n;
      loop ()
    end
  in
  loop ()

module Miou_process = struct
  type redirection = [
    | `Keep
    | `Dev_null
    | `Close
    | `FD_copy of Miou_unix.file_descr
    | `FD_move of Miou_unix.file_descr
  ]

  let with_process_none ~stdin:_ ~stdout:_ ~stderr:_ _cmd _f =
    assert false (* TODO *)

  let with_process_in ?cwd:_ ~timeout:_ ~stdin:_ _cmd _f =
    assert false (* TODO *)

  let exec ~stdin:_ ~stdout:_ ~stderr:_ _cmd =
    assert false (* TODO *)
end

module Miou_pool = struct
  let create _n _f =
    assert false (* TODO *)

  let use _pool _f =
    assert false (* TODO *)
end

module Miou_list = struct
  let map_p f l =
    List.map (fun x -> Miou.await_exn x)
      (List.map (fun x -> Miou.async (fun () -> f x)) l)
end
