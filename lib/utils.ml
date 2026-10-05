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

  let redirection kind redirection = match kind, redirection with
    | `Stdin, `Keep -> Unix.stdin
    | `Stdout, `Keep -> Unix.stdout
    | `Stderr, `Keep -> Unix.stderr
    | `Stdin, `Dev_null -> Unix.openfile Filename.null [Unix.O_RDONLY] 0
    | (`Stdout | `Stderr), `Dev_null -> Unix.openfile Filename.null [Unix.O_WRONLY] 0
    | `Stdin, `Close ->
        let rd, wr = Unix.pipe ~cloexec:true () in
        Unix.close wr;
        rd
    | (`Stdout | `Stderr), `Close ->
        let rd, wr = Unix.pipe ~cloexec:true () in
        Unix.close rd;
        wr
    | _, `FD_copy fd -> Unix.dup ~cloexec:false (Miou_unix.to_file_descr fd)
    | _, `FD_move fd -> Unix.dup ~cloexec:true (Miou_unix.to_file_descr fd)

  let exec_aux ~stdin ~stdout ~stderr ((cmd, args) as cmdargs) =
    let stdin = redirection `Stdin stdin in
    let stdout = redirection `Stdout stdout in
    let stderr = redirection `Stderr stderr in
    let cmd, args =
      match args with
      | [||] -> (cmd, [|cmd|])
      | _ -> cmdargs
    in
    Unix.create_process cmd args stdin stdout stderr

  let exec ~stdin ~stdout ~stderr cmdargs =
    let pid = exec_aux ~stdin ~stdout ~stderr cmdargs in
    snd (Unix.waitpid [] pid)

  let with_process_none ~stdin ~stdout ~stderr cmdargs f =
    let pid = exec_aux ~stdin ~stdout ~stderr cmdargs in
    f (object
      method close = snd (Unix.waitpid [] pid)
      method terminate = Unix.kill pid Sys.sigkill
    end)

  let with_process_in ?cwd:_ ~timeout:_ ~stdin ((cmd, args) as cmdargs) f =
    let stdin = redirection `Stdin stdin in
    let pipe, stdout = Unix.pipe ~cloexec:true () in
    let stderr = Unix.stderr in
    let cmd, args =
      match args with
      | [||] -> (cmd, [|cmd|])
      | _ -> cmdargs
    in
    let pid = Unix.create_process cmd args stdin stdout stderr in
    f object
      method close = snd (Unix.waitpid [] pid)
      method stdout = Miou_unix.of_file_descr pipe
    end
end

module Miou_pool = struct
  let create n f =
    Cattery.create n f

  let use pool f =
    Cattery.use pool (fun () -> Miou.async f)
end

module Miou_list = struct
  let map_p f l =
    List.map (fun x -> Miou.await_exn x)
      (List.map (fun x -> Miou.async (fun () -> f x)) l)
end
