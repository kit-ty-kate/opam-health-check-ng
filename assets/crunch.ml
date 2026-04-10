let sanitise str =
  let buf = Buffer.create 64 in
  for i = 0 to String.length str - 1 do
    let c = str.[i] in
    if Char.Ascii.is_lower c then
      Buffer.add_char buf c
    else
      Buffer.add_char buf '_'
  done;
  Buffer.contents buf

let crunch filename =
  let var_name = sanitise (Filename.basename filename) in
  let content = In_channel.with_open_bin filename In_channel.input_all in
  Printf.printf "let %s = %S\n" var_name content

let () =
  assert (Array.length Sys.argv = 2);
  let dirname = Sys.argv.(1) in
  Array.iter (fun filename ->
    let filename = Filename.concat dirname filename in
    if String.ends_with ~suffix:".png" filename then
      crunch filename
  ) (Sys.readdir dirname)
