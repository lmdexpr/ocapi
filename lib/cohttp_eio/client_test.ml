(* The handler is checked against a real cohttp-eio server on the loopback interface. *)

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
  f (Uri.make ~scheme:"http" ~host:"127.0.0.1" ~port ())

let echo =
  Ocapi.Endpoint.Response.json ~status:200 ~description:"echo"
    (Ocapi.Endpoint.Codec.make ~name:"echo" ~schema:(`Assoc [])
       (fun s -> `String s)
       Yojson.Safe.Util.to_string)
    Ocapi.Endpoint.Prism.id

type ping = { id : string; year : int option; note : string }

let ping =
  let open Ocapi.Endpoint in
  let open Syntax in
  make ~meth:`POST "/ping/{id}" ~tags:[] ~summary:"s"
    (let+ id = Path.string "id" (fun r -> r.id)
     and+ year =
       Query.optional ~description:"" (Value.int ()) "year" (fun r -> r.year)
       |> Provide.require (fun _ -> Reply.empty 400)
     and+ note =
       Body.raw ~name:"note" ~schema:(`Assoc []) ~content_type:"text/plain" (fun r -> r.note)
     in
     { id; year; note })
    ~responses:[ echo ]

let ping_a1 = { id = "a1"; year = None; note = "" }

let run ~env ~base ?max_body_size f =
  let client = Cohttp_eio.Client.make ~https:None env#net in
  Ocapi_cohttp_eio.Client.run ~client ~base ?max_body_size f

(* The server sees the path under the base, the query appended to the base's, the bearer header and
   the body. *)
let test_the_request_reaches_the_server () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let seen = ref None in
  let callback _conn request body =
    let body = Eio.Buf_read.of_flow ~max_size:1_000_000 body |> Eio.Buf_read.take_all in
    seen :=
      Some
        ( Http.Request.resource request,
          Http.Header.get (Http.Request.headers request) "authorization",
          body );
    Cohttp_eio.Server.respond_string ~status:`OK ~body:{|"pong"|} ()
  in
  with_server ~sw env ~callback @@ fun base ->
  let base = Uri.with_query (Uri.with_path base "/api/v1/") [ "from_base", [ "1" ] ] in
  let result =
    run ~env ~base @@ fun () ->
    Ocapi.Client.call ~bearer:"t" ping { id = "a1"; year = Some 2026; note = "{}" }
  in
  (match result with
  | Ok echoed -> Alcotest.(check string) "decoded" "pong" echoed
  | Error _ -> Alcotest.fail "expected Ok");
  Alcotest.(check (option (triple string (option string) string)))
    "what the server saw"
    (Some ("/api/v1/ping/a1?from_base=1&year=2026", Some "Bearer t", "{}"))
    !seen

let test_statuses_without_a_case () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let callback _conn request _body =
    let status =
      match Http.Request.resource request with
      | "/ping/u" -> `Unauthorized
      | _ -> `Internal_server_error
    in
    Cohttp_eio.Server.respond_string ~status ~body:"boom" ()
  in
  with_server ~sw env ~callback @@ fun base ->
  run ~env ~base @@ fun () ->
  let call id = Ocapi.Client.call ping { ping_a1 with id } in
  (match call "u" with
  | Error `Unauthorized -> ()
  | Ok _ | Error _ -> Alcotest.fail "expected Unauthorized");
  match call "x" with
  | Error (`Undeclared { status = 500; body = "boom"; _ }) -> ()
  | Ok _ | Error _ -> Alcotest.fail "expected Undeclared 500"

(* Failures raise at the perform site: a response over the limit, and a connection refused. *)
let test_failures_raise_where_the_request_was_performed () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let callback _conn _request _body =
    Cohttp_eio.Server.respond_string ~status:`OK ~body:(String.make 100 'x') ()
  in
  with_server ~sw env ~callback @@ fun base ->
  let call () = Ocapi.Client.call ping ping_a1 in
  ( run ~env ~base ~max_body_size:16 @@ fun () ->
    match call () with
    | _ -> Alcotest.fail "expected Buffer_limit_exceeded"
    | exception Eio.Buf_read.Buffer_limit_exceeded -> () );
  let unused =
    Eio.Switch.run @@ fun sw ->
    let socket =
      Eio.Net.listen env#net ~sw ~backlog:1 ~reuse_addr:true (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> Uri.make ~scheme:"http" ~host:"127.0.0.1" ~port ()
    | `Unix _ -> assert false
  in
  run ~env ~base:unused @@ fun () ->
  match call () with _ -> Alcotest.fail "expected a connection failure" | exception Eio.Io _ -> ()

(* A request built by hand bypasses [Ocapi.Client.call]'s checks; the handler refuses it. *)
let test_a_header_with_a_line_break_is_refused () =
  Eio_main.run @@ fun env ->
  let base = Uri.of_string "http://127.0.0.1:1" in
  run ~env ~base @@ fun () ->
  let request = Ocapi.Client.request { meth = `GET; path = "/" } in
  match Ocapi.Client.send { request with headers = [ "x-a", "1\r\nx-b: 2" ] } with
  | _ -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let () =
  Alcotest.run "cohttp_eio client"
    [
      ( "run",
        [
          Alcotest.test_case "the request reaches the server" `Quick
            test_the_request_reaches_the_server;
          Alcotest.test_case "statuses without a case" `Quick test_statuses_without_a_case;
          Alcotest.test_case "failures raise where the request was performed" `Quick
            test_failures_raise_where_the_request_was_performed;
          Alcotest.test_case "a header with a line break is refused" `Quick
            test_a_header_with_a_line_break_is_refused;
        ] );
    ]
