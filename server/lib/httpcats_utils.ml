let drain_body = function
  | `V1 reqd ->
      let reader = H1.Reqd.request_body reqd in
      H1.Body.Reader.close reader
  | `V2 reqd ->
      let reader = H2.Reqd.request_body reqd in
      H2.Body.Reader.close reader

let target = function
  | `V1 reqd ->
      let req = H1.Reqd.request reqd in
      req.H1.Request.target
  | `V2 reqd ->
      let req = H2.Reqd.request reqd in
      req.H2.Request.target

let respond ~status reqd body =
  let headers, body = match body with
    | `Empty -> ([("content-length", "0")], "")
    | (`Plain_text body | `Html body | `Json body) as content_type ->
        let content_type = match content_type with
          | `Plain_text _ -> "text/plain; charset=utf-8"
          | `Html _ -> "text/html; charset=utf-8"
          | `Json _ -> "application/json; charset=utf-8"
        in
        ([
          ("content-type", content_type);
          ("content-length", string_of_int (String.length body));
        ], body)
  in
  match reqd with
  | `V1 reqd ->
      let headers = H1.Headers.of_list headers in
      let resp = H1.Response.create ~headers status in
      H1.Reqd.respond_with_string reqd resp body
  | `V2 reqd ->
      let headers = H2.Headers.of_list headers in
      let resp = H2.Response.create ~headers (status :> H2.Status.t) in
      H2.Reqd.respond_with_string reqd resp body

let respond_stream ~status reqd f =
  match reqd with
  | `V1 reqd ->
      let headers =
        H1.Headers.of_list [
          ("content-type", "text/plain; charset=utf-8");
        ]
      in
      let resp = H1.Response.create ~headers status in
      let writer = H1.Reqd.respond_with_streaming ~flush_headers_immediately:true reqd resp in
      f (fun x -> H1.Body.Writer.write_string writer x);
      H1.Body.Writer.close writer
  | `V2 reqd ->
      let headers =
        H2.Headers.of_list [
          ("content-type", "text/plain; charset=utf-8");
        ]
      in
      let resp = H2.Response.create ~headers (status :> H2.Status.t) in
      let writer = H2.Reqd.respond_with_streaming ~flush_headers_immediately:true reqd resp in
      f (fun x -> H2.Body.Writer.write_string writer x);
      H2.Body.Writer.close writer

let read_body = function
  | `V1 reqd ->
      let reader = H1.Reqd.request_body reqd in
      let buf = Buffer.create 4096 in
      let rec loop buf =
        H1.Body.Reader.schedule_read reader
          ~on_eof:(fun () -> H1.Body.Reader.close reader)
          ~on_read:(fun bstr ~off ~len ->
            let str = Bigstringaf.substring bstr ~off ~len in
            Buffer.add_string buf str;
            loop buf)
      in
      loop buf;
      Buffer.contents buf
  | `V2 reqd ->
      let reader = H2.Reqd.request_body reqd in
      let buf = Buffer.create 4096 in
      let rec loop buf =
        H2.Body.Reader.schedule_read reader
          ~on_eof:(fun () -> H2.Body.Reader.close reader)
          ~on_read:(fun bstr ~off ~len ->
            let str = Bigstringaf.substring bstr ~off ~len in
            Buffer.add_string buf str;
            loop buf)
      in
      loop buf;
      Buffer.contents buf
