type callback =
  Cohttp_eio.Server.conn -> Http.Request.t -> Cohttp_eio.Body.t -> Cohttp_eio.Server.response

exception Connection_closed of string

(* A body that remembers reaching its end: cohttp's chunked reader, read once more after the end,
   takes the next request line off the socket. *)
module Tracked = struct
  type t = { body : Cohttp_eio.Body.t; mutable eof : bool }

  let single_read t buffer =
    if t.eof then raise End_of_file;
    match Eio.Flow.single_read t.body buffer with
    | n -> n
    | exception End_of_file ->
      t.eof <- true;
      raise End_of_file

  let read_methods = []
end

let tracked = Eio.Flow.Pi.source (module Tracked)

let drain ~limit (t : Tracked.t) =
  let buffer = Cstruct.create 4096 in
  let rec go read =
    if not t.eof then
      match
        Tracked.single_read t buffer
      with
      | n when read + n > limit -> raise (Connection_closed "unread request body over the limit")
      | n -> go (read + n)
      | exception End_of_file -> ()
  in
  go 0

let digits s = s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s

(* cohttp keeps a bare LF inside a header value, so a front proxy that ends lines at LF and
   cohttp would see different headers (a hidden Transfer-Encoding, say). *)
let one_line s = not (String.exists (function '\r' | '\n' | '\000' -> true | _ -> false) s)

(* The framing is decided here from the headers as sent, strictly (RFC 9112 §6), never from what
   cohttp makes of them: cohttp ignores a body on GET / HEAD, takes a Transfer-Encoding other than
   exactly [chunked] as no framing at all, derives a length from Content-Range, and reads a body
   without framing up to the end of the connection. Each of those leaves bytes that a front proxy
   and cohttp would split into requests differently. *)
let framing request =
  let headers = Http.Request.headers request in
  let lengths = Http.Header.get_multi headers "content-length" in
  let encodings = Http.Header.get_multi headers "transfer-encoding" in
  let body_allowed = Http.Method.body_allowed (Http.Request.meth request) in
  let length s = if digits s then Int64.of_string_opt s else None in
  if
    not
      (Http.Header.to_list headers
      |> List.for_all (fun (name, value) -> one_line name && one_line value))
  then
    `Reject "a line break inside a header"
  else
    match
      lengths, encodings
    with
    | [], [] -> `Empty
    | _ :: _, _ :: _ -> `Reject "both Content-Length and Transfer-Encoding"
    | [], [ "chunked" ] when body_allowed -> `Body
    | [], _ :: _ -> `Reject "a Transfer-Encoding other than chunked, or on a method without a body"
    | [ value ], [] -> (
      match length value with
      | Some 0L -> `Empty
      | Some _ when body_allowed -> `Body
      | Some _ -> `Reject "a body on a method without one"
      | None -> `Reject "an invalid Content-Length")
    | _ :: _ :: _, [] -> `Reject "several Content-Length headers"

let close_header = Http.Header.init_with "connection" "close"

(* The closure the callback returns writes the response; draining comes after it, so a rejected
   request is answered before its body has arrived. An exception from the callback closes the
   connection, so nothing is left to drain. *)
let draining ?(max_unread = 1_000_000) callback conn request body =
  match framing request with
  | `Reject reason ->
    fun writer ->
      Cohttp_eio.Server.respond_string ~status:`Bad_request ~headers:close_header ~body:"" () writer;
      raise (Connection_closed ("rejected a request with " ^ reason))
  | (`Empty | `Body) as framing ->
    let body = match framing with `Empty -> Eio.Flow.string_source "" | `Body -> body in
    let t = Tracked.{ body; eof = false } in
    let respond = callback conn request (Eio.Resource.T (t, tracked)) in
    fun writer ->
      respond writer;
      drain ~limit:max_unread t

let default_max_body_size = 1_000_000

let request ?(max_body_size = default_max_body_size) request body =
  match Http.Request.meth request with
  | `HEAD | `OPTIONS | `TRACE | `CONNECT | `Other _ -> None
  | #Ocapi.Endpoint.meth as meth ->
    let uri = Uri.of_string (Http.Request.resource request) in
    Some
      Ocapi.Endpoint.Request.
        {
          meth;
          segments = String.split_on_char '/' (Uri.path uri) |> List.filter (fun s -> s <> "");
          query = Uri.query uri;
          header = Http.Header.get (Http.Request.headers request);
          body =
            (* Read once: a second read of a finished chunked body would take the next request
               off the connection. *)
            (let read =
               lazy (Eio.Buf_read.of_flow ~max_size:max_body_size body |> Eio.Buf_read.take_all)
             in
             fun () -> Lazy.force read);
          params = [];
        }

(* Framing is cohttp's; a handler's value there would desync the stream. *)
let framing_header name =
  match String.lowercase_ascii name with
  | "content-length" | "transfer-encoding" -> true
  | _ -> false

let headers (reply : Ocapi.Endpoint.Reply.t) =
  reply.headers
  |> List.filter (fun (name, _) -> not (framing_header name))
  |> List.map (fun (name, value) ->
    if one_line name && one_line value then
      name, value
    else
      invalid_arg ("header " ^ name ^ " spans several lines"))
  |> Http.Header.of_list

(* The length is always explicit: without it, cohttp takes a Content-Range from the handler as the
   framing and sends neither Content-Length nor Transfer-Encoding. *)
let respond (reply : Ocapi.Endpoint.Reply.t) =
  let status = Http.Status.of_int reply.status in
  let headers = headers reply in
  let send ~headers body =
    Cohttp_eio.Server.respond_string ~status
      ~headers:(Http.Header.replace headers "content-length" (string_of_int (String.length body)))
      ~body ()
  in
  match reply.body with
  | Empty -> send ~headers ""
  | Raw body -> send ~headers body
  | Json json ->
    send
      ~headers:(Http.Header.add_unless_exists headers "content-type" "application/json")
      (Yojson.Safe.to_string json)

let callback ?max_body_size ?(on_exception = fun _ -> Ocapi.Endpoint.Reply.empty 500) serve _conn
  http body =
  match request ?max_body_size http body with
  | None -> respond (Ocapi.Endpoint.Reply.empty 404)
  | Some request -> (
    match serve request |> respond with
    | response -> response
    | exception Eio.Buf_read.Buffer_limit_exceeded -> respond (Ocapi.Endpoint.Reply.empty 413)
    | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
    | exception exn -> respond (on_exception exn))

let run ~net ?(addr = Eio.Net.Ipaddr.V4.loopback) ~port ?max_connections ?max_body_size ?max_unread
  ?on_exception ?(on_connection_error = ignore) serve =
  Eio.Switch.run @@ fun sw ->
  let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true (`Tcp (addr, port)) in
  let callback = callback ?max_body_size ?on_exception serve |> draining ?max_unread in
  Cohttp_eio.Server.run ?max_connections socket
    (Cohttp_eio.Server.make ~callback ())
    ~on_error:on_connection_error

let common : Ocapi.Endpoint.common list =
  [
    {
      status = 400;
      description = "The request is framed ambiguously (a body on GET, conflicting lengths)";
      scope = `Every_operation;
    };
    { status = 413; description = "The request body is too large"; scope = `Operations_with_body };
    { status = 500; description = "Internal error"; scope = `Every_operation };
  ]
