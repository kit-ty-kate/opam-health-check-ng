let with_in file f =
  let fd = Unix.openfile file [Unix.O_RDONLY] 0o640 in
  let fd = try Miou_unix.of_file_descr fd with e -> Unix.close fd; raise e in
  Fun.protect ~finally:(fun () -> Miou_unix.close fd) (fun () -> f fd)

module Miou_process = struct
  type redirection

  let with_process_none ~stdin:_ ~stdout:_ ~stderr:_ _cmd _f =
    assert false (* TODO *)

  let with_process_in ?cwd:_ ~timeout:_ ~stdin:_ _cmd _f =
    assert false (* TODO *)

  let exec ~stdin:_ ~stdout:_ ~stderr:_ _cmd =
    assert false (* TODO *)
end
