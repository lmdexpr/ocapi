(* A handler that answers without reading the body, checked with two requests on one connection.
   Without draining, the first body is read as the second request, which becomes a 400. *)

let ignores_the_body _conn _request _body =
  Cohttp_eio.Server.respond_string ~status:`Unauthorized ~body:"" ()

let draining = Ocapi_cohttp_eio.Server.draining ignores_the_body

(* The accept loop never returns on its own, so it runs as a daemon and is cancelled when the test
   body returns. *)
let with_server ~sw env ~callback f =
  let socket =
    Eio.Net.listen env#net ~sw ~backlog:5 ~reuse_addr:true (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with `Tcp (_, port) -> port | `Unix _ -> assert false
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    (Cohttp_eio.Server.run socket (Cohttp_eio.Server.make ~callback ()) ~on_error:(fun _ -> ())
      : unit);
    `Stop_daemon);
  f port

let status_lines ~sw env port raw =
  let flow = Eio.Net.connect ~sw env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  Eio.Flow.copy_string raw flow;
  Eio.Time.with_timeout_exn env#clock 2.0 @@ fun () ->
  Eio.Buf_read.of_flow ~max_size:1_000_000 flow
  |> Eio.Buf_read.lines
  |> Seq.filter (String.starts_with ~prefix:"HTTP/1.1 ")
  |> List.of_seq

let pipelined =
  "POST /a HTTP/1.1\r\nHost: t\r\nContent-Length: 20\r\n\r\nGET /evil HTTP/1.1\r\n"
  ^ "GET /b HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n"

let test_the_second_request_is_served_after_an_unread_body () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_server ~sw env ~callback:draining @@ fun port ->
  Alcotest.(check (list string))
    "both requests get the handler's answer"
    [ "HTTP/1.1 401 Unauthorized"; "HTTP/1.1 401 Unauthorized" ]
    (status_lines ~sw env port pipelined)

(* Without draining the second request breaks: cohttp-eio gives no such guarantee itself. *)
let test_without_draining_the_second_request_breaks () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_server ~sw env ~callback:ignores_the_body @@ fun port ->
  Alcotest.(check (list string))
    "the leftover body is read as the next request"
    [ "HTTP/1.1 401 Unauthorized"; "HTTP/1.1 400 Bad Request" ]
    (status_lines ~sw env port pipelined)

(* A rejection is answered before the body is complete. *)
let test_the_response_arrives_before_the_body_is_complete () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_server ~sw env ~callback:draining @@ fun port ->
  let flow = Eio.Net.connect ~sw env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  Eio.Flow.copy_string "POST /a HTTP/1.1\r\nHost: t\r\nContent-Length: 20\r\n\r\nhalf" flow;
  let input = Eio.Buf_read.of_flow ~max_size:1_000_000 flow in
  Alcotest.(check string)
    "status line before the body ends" "HTTP/1.1 401 Unauthorized"
    (Eio.Time.with_timeout_exn env#clock 2.0 @@ fun () -> Eio.Buf_read.line input)

let reads_the_body _conn _request body =
  ignore (Eio.Buf_read.of_flow ~max_size:1_000_000 body |> Eio.Buf_read.take_all);
  Cohttp_eio.Server.respond_string ~status:`OK ~body:"" ()

(* Reading a chunked body once more after its end makes cohttp eat the next request line. A body
   read to its end is left alone by the draining, checked with a GET after a chunked POST. *)
let test_a_body_read_to_the_end_is_not_read_again () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_server ~sw env ~callback:(Ocapi_cohttp_eio.Server.draining reads_the_body) @@ fun port ->
  Alcotest.(check (list string))
    "both requests are answered"
    [ "HTTP/1.1 200 OK"; "HTTP/1.1 200 OK" ]
    (status_lines ~sw env port
       ("POST /a HTTP/1.1\r\nHost: t\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n0\r\n\r\n"
      ^ "GET /b HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n"))

(* A POST with neither Content-Length nor Transfer-Encoding has an empty body; cohttp would read
   up to the end of the connection, swallowing the GET that follows. *)
let test_a_request_without_a_length_has_an_empty_body () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_server ~sw env ~callback:(Ocapi_cohttp_eio.Server.draining reads_the_body) @@ fun port ->
  Alcotest.(check (list string))
    "both requests are answered"
    [ "HTTP/1.1 200 OK"; "HTTP/1.1 200 OK" ]
    (status_lines ~sw env port
       ("POST /a HTTP/1.1\r\nHost: t\r\n\r\n"
      ^ "GET /b HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n"))

(* A body on a GET is not a body cohttp reads; its bytes would become the next request. The request
   is refused and the connection closed, so the smuggled request is never answered. *)
let test_a_body_on_a_get_closes_the_connection () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_server ~sw env ~callback:(Ocapi_cohttp_eio.Server.draining reads_the_body) @@ fun port ->
  Alcotest.(check (list string))
    "one 400, nothing for the smuggled request" [ "HTTP/1.1 400 Bad Request" ]
    (status_lines ~sw env port
       ("GET /a HTTP/1.1\r\nHost: t\r\nContent-Length: 27\r\n\r\n"
      ^ "GET /evil HTTP/1.1\r\nHost: t\r\n\r\n"))

(* Framing cohttp would misread is refused rather than guessed at. *)
let test_ambiguous_framing_is_refused () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_server ~sw env ~callback:(Ocapi_cohttp_eio.Server.draining reads_the_body) @@ fun port ->
  [
    "Transfer-Encoding: gzip, chunked\r\nContent-Length: 4";
    "Transfer-Encoding: chunked\r\nContent-Length: 4";
    "Content-Length: 4\r\nContent-Length: 5";
    "Content-Length: 0x10";
    "Content-Length: 99999999999999999999";
    (* cohttp takes only exactly [chunked] as chunked, and reads anything else to the end. *)
    "Transfer-Encoding: Chunked";
    "Transfer-Encoding:  chunked ,";
    (* A proxy ending lines at LF sees a Transfer-Encoding that cohttp keeps inside X-A. *)
    "X-A: a\nTransfer-Encoding: chunked";
  ]
  |> List.iter @@ fun headers ->
     Alcotest.(check (list string))
       headers [ "HTTP/1.1 400 Bad Request" ]
       (status_lines ~sw env port ("POST /a HTTP/1.1\r\nHost: t\r\n" ^ headers ^ "\r\n\r\nabcd"))

(* cohttp derives a body length from a request's Content-Range; a proxy does not. Without
   Content-Length or Transfer-Encoding the body is empty, so all three requests are answered. *)
let test_content_range_is_not_a_length () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_server ~sw env ~callback:(Ocapi_cohttp_eio.Server.draining reads_the_body) @@ fun port ->
  Alcotest.(check (list string))
    "three answers"
    [ "HTTP/1.1 200 OK"; "HTTP/1.1 200 OK"; "HTTP/1.1 200 OK" ]
    (status_lines ~sw env port
       ("POST /a HTTP/1.1\r\nHost: t\r\nContent-Range: bytes 0-21/100\r\n\r\n"
      ^ "GET /b HTTP/1.1\r\nHost: t\r\n\r\n"
      ^ "GET /c HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n"))

(* Draining stops at [max_unread]; the connection is closed instead of reading a huge body. *)
let test_an_unread_body_over_the_limit_closes_the_connection () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_server ~sw env ~callback:(Ocapi_cohttp_eio.Server.draining ~max_unread:100 ignores_the_body)
  @@ fun port ->
  Alcotest.(check (list string))
    "answered once, then closed" [ "HTTP/1.1 401 Unauthorized" ]
    (status_lines ~sw env port
       ("POST /a HTTP/1.1\r\nHost: t\r\nContent-Length: 1000\r\n\r\n" ^ String.make 1000 'x'
      ^ "GET /b HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n"))

(* The callback: an endpoint served end to end, with the status codes the adapter decides. *)
let routes =
  let echo =
    Ocapi.Endpoint.Response.json ~status:200 ~description:"echo"
      (Ocapi.Endpoint.Codec.make ~name:"echo" ~schema:(`Assoc [])
         (fun s -> `String s)
         Yojson.Safe.Util.to_string)
      Ocapi.Endpoint.Prism.id
  in
  let open Ocapi.Endpoint.Syntax in
  [
    Ocapi.Endpoint.make ~meth:`POST "/items/{id}" ~tags:[] ~summary:"s"
      (let+ id = Ocapi.Endpoint.Path.string "id" (fun (id, _, _) -> id)
       and+ q =
         Ocapi.Endpoint.Query.required ~description:"" Ocapi.Endpoint.Value.string "q"
           (fun (_, q, _) -> q)
         |> Ocapi.Endpoint.Provide.require (fun _ -> Ocapi.Endpoint.Reply.empty 400)
       and+ body =
         Ocapi.Endpoint.Body.raw ~name:"b" ~schema:(`Assoc []) ~content_type:"text/plain"
           (fun (_, _, b) -> b)
       in
       id, q, body)
      ~responses:[ echo ]
    |> Ocapi.Server.handle (fun (id, q, body) -> String.concat " " [ id; q; body ]);
    Ocapi.Endpoint.make ~meth:`GET "/boom" ~tags:[] ~summary:"s"
      (Ocapi.Endpoint.Provide.return ())
      ~responses:[ echo ]
    |> Ocapi.Server.handle (fun () -> failwith "boom");
    Ocapi.Endpoint.make ~meth:`GET "/headers" ~tags:[] ~summary:"s"
      (Ocapi.Endpoint.Provide.return ())
      ~responses:
        [
          Ocapi.Endpoint.Response.raw ~status:200 ~description:""
            (Ocapi.Endpoint.Prism.make ~inject:ignore ~project:(fun () ->
               Some ([ "content-length", "999"; "x-user", "bob" ], "ok")));
        ]
    |> Ocapi.Server.handle Fun.id;
    Ocapi.Endpoint.make ~meth:`GET "/split" ~tags:[] ~summary:"s"
      (Ocapi.Endpoint.Provide.return ())
      ~responses:
        [
          Ocapi.Endpoint.Response.raw ~status:200 ~description:""
            (Ocapi.Endpoint.Prism.make ~inject:ignore ~project:(fun () ->
               Some ([ "x-user", "bob\r\nx-injected: yes" ], "ok")));
        ]
    |> Ocapi.Server.handle Fun.id;
  ]

let served =
  Ocapi_cohttp_eio.Server.draining
    (Ocapi_cohttp_eio.Server.callback ~max_body_size:16 (Ocapi.Server.serve routes))

let request ~sw env port ?(headers = []) ?(body = "") meth path =
  let client = Cohttp_eio.Client.make ~https:None env#net in
  let response, reader =
    Cohttp_eio.Client.call ~sw client meth ~headers:(Http.Header.of_list headers)
      ~body:(Cohttp_eio.Body.of_string body)
      (Uri.of_string (Printf.sprintf "http://127.0.0.1:%d%s" port path))
  in
  ( Http.Status.to_int (Http.Response.status response),
    Http.Header.get (Http.Response.headers response) "content-type",
    Eio.Buf_read.of_flow ~max_size:1_000_000 reader |> Eio.Buf_read.take_all )

(* A [serve] wrapper (logging, say) may read the body before the endpoint does. The second read of
   a finished chunked body must not reach into the next request. *)
let test_a_body_read_twice_leaves_the_next_request () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let serve (request : Ocapi.Endpoint.Request.t) =
    let first = request.body () in
    let second = request.body () in
    Ocapi.Endpoint.Reply.
      { status = 200; headers = [ "x-read", first ^ "|" ^ second ]; body = Empty }
  in
  with_server ~sw env
    ~callback:(Ocapi_cohttp_eio.Server.draining (Ocapi_cohttp_eio.Server.callback serve))
  @@ fun port ->
  let flow = Eio.Net.connect ~sw env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  Eio.Flow.copy_string
    ("POST /a HTTP/1.1\r\nHost: t\r\nTransfer-Encoding: chunked\r\n\r\n4\r\n\"hi\"\r\n0\r\n\r\n"
   ^ "GET /b HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n")
    flow;
  let lines =
    Eio.Time.with_timeout_exn env#clock 2.0 @@ fun () ->
    Eio.Buf_read.of_flow ~max_size:1_000_000 flow |> Eio.Buf_read.lines |> List.of_seq
  in
  Alcotest.(check (list string))
    "both answered, the second read gives the same body"
    [ "HTTP/1.1 200 OK"; {|x-read: "hi"|"hi"|}; "HTTP/1.1 200 OK"; "x-read: |" ]
    (lines
    |> List.filter (fun line ->
      String.starts_with ~prefix:"HTTP/1.1 " line || String.starts_with ~prefix:"x-read" line))

(* A Content-Range from the handler would otherwise leave the response without a length. *)
let test_a_content_range_reply_carries_its_length () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let serve _ =
    Ocapi.Endpoint.Reply.
      { status = 206; headers = [ "content-range", "bytes 0-1/100" ]; body = Raw "hello world" }
  in
  with_server ~sw env
    ~callback:(Ocapi_cohttp_eio.Server.draining (Ocapi_cohttp_eio.Server.callback serve))
  @@ fun port ->
  let flow = Eio.Net.connect ~sw env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  Eio.Flow.copy_string "GET /a HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n" flow;
  let lines =
    Eio.Time.with_timeout_exn env#clock 2.0 @@ fun () ->
    Eio.Buf_read.of_flow ~max_size:1_000_000 flow |> Eio.Buf_read.lines |> List.of_seq
  in
  Alcotest.(check bool)
    "content-length: 11" true
    (List.exists (fun line -> String.lowercase_ascii line = "content-length: 11") lines)

let test_callback_serves_the_routes () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_server ~sw env ~callback:served @@ fun port ->
  let request = request ~sw env port in
  Alcotest.(check (triple int (option string) string))
    "path, query and body reach the handler; JSON is typed"
    (200, Some "application/json", {|"a1 x hi"|})
    (request ~body:"hi" ~headers:[ "content-length", "2" ] `POST "/items/a1?q=x");
  Alcotest.(check (triple int (option string) string))
    "no route" (404, None, "") (request `GET "/nowhere");
  Alcotest.(check (triple int (option string) string))
    "a method without endpoints" (404, None, "") (request `OPTIONS "/items/a1");
  Alcotest.(check (triple int (option string) string))
    "a body over the limit" (413, None, "")
    (request ~body:(String.make 17 'x') `POST "/items/a1?q=x");
  Alcotest.(check (triple int (option string) string))
    "an exception from the handler" (500, None, "") (request `GET "/boom");
  Alcotest.(check (triple int (option string) string))
    "the handler's framing header is dropped, the rest kept" (200, None, "ok")
    (request `GET "/headers");
  Alcotest.(check (triple int (option string) string))
    "a header spanning lines is an error, not a split response" (500, None, "")
    (request `GET "/split")

(* [run] serves an ocapi function; the port is one the system just handed out and released. *)
let test_run_serves_the_function () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let port =
    Eio.Switch.run @@ fun sw ->
    let socket =
      Eio.Net.listen env#net ~sw ~backlog:1 ~reuse_addr:true (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    match Eio.Net.listening_addr socket with `Tcp (_, port) -> port | `Unix _ -> assert false
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Ocapi_cohttp_eio.Server.run ~net:env#net ~addr:Eio.Net.Ipaddr.V4.loopback ~port
      (Ocapi.Server.serve routes));
  Alcotest.(check (triple int (option string) string))
    "served"
    (200, Some "application/json", {|"a1 x "|})
    (request ~sw env port ~headers:[ "content-length", "0" ] `POST "/items/a1?q=x")

let () =
  Alcotest.run "cohttp_eio server"
    [
      ( "drain",
        [
          Alcotest.test_case "the second request is served after an unread body" `Quick
            test_the_second_request_is_served_after_an_unread_body;
          Alcotest.test_case "without draining the second request breaks" `Quick
            test_without_draining_the_second_request_breaks;
          Alcotest.test_case "the response arrives before the body is complete" `Quick
            test_the_response_arrives_before_the_body_is_complete;
          Alcotest.test_case "a body read to the end is not read again" `Quick
            test_a_body_read_to_the_end_is_not_read_again;
          Alcotest.test_case "a request without a length has an empty body" `Quick
            test_a_request_without_a_length_has_an_empty_body;
          Alcotest.test_case "a body on a GET closes the connection" `Quick
            test_a_body_on_a_get_closes_the_connection;
          Alcotest.test_case "ambiguous framing is refused" `Quick test_ambiguous_framing_is_refused;
          Alcotest.test_case "Content-Range is not a length" `Quick
            test_content_range_is_not_a_length;
          Alcotest.test_case "a body read twice leaves the next request" `Quick
            test_a_body_read_twice_leaves_the_next_request;
          Alcotest.test_case "a Content-Range reply carries its length" `Quick
            test_a_content_range_reply_carries_its_length;
          Alcotest.test_case "an unread body over the limit closes the connection" `Quick
            test_an_unread_body_over_the_limit_closes_the_connection;
        ] );
      ( "callback",
        [
          Alcotest.test_case "serves the routes" `Quick test_callback_serves_the_routes;
          Alcotest.test_case "run serves the function" `Quick test_run_serves_the_function;
        ] );
    ]
