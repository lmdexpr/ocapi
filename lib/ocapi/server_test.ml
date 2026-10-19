let request ?(meth = `GET) path : Ocapi.Endpoint.Request.t =
  {
    meth;
    segments = String.split_on_char '/' path |> List.filter (fun s -> s <> "");
    query = [];
    header = (fun _ -> None);
    body = (fun () -> "");
    params = [];
  }

let reply =
  Alcotest.testable
    (fun fmt ({ status; headers; body } : Ocapi.Endpoint.Reply.t) ->
      Format.fprintf fmt "%d [%s] %s" status
        (String.concat "; " (List.map (fun (k, v) -> k ^ ": " ^ v) headers))
        (match body with
        | Empty -> "empty"
        | Raw s -> "raw " ^ s
        | Json j -> Yojson.Safe.to_string j))
    ( = )

let echo =
  Ocapi.Endpoint.Response.json ~status:200 ~description:"echo"
    (Ocapi.Endpoint.Codec.make ~name:"echo" ~schema:(`Assoc [])
       (fun s -> `String s)
       Yojson.Safe.Util.to_string)
    Ocapi.Endpoint.Prism.id

let endpoint ?(meth = `GET) ?headers template provide responses =
  Ocapi.Endpoint.make ~meth template ?headers ~tags:[] ~summary:"s" provide ~responses

(* A [{name}] segment matches one segment; a literal matches only itself. *)
let test_dispatch_matches_the_template () =
  let items = endpoint "/api/items/{id}/tags" (Ocapi.Endpoint.Path.string "id" Fun.id) [ echo ] in
  let dispatch ?meth path =
    Ocapi.Server.dispatch [ items |> Ocapi.Server.handle Fun.id ] (request ?meth path)
  in
  Alcotest.(check (option reply))
    "param"
    (Some (Ocapi.Endpoint.Reply.json 200 (`String "a1")))
    (dispatch "/api/items/a1/tags");
  Alcotest.(check (option reply)) "literal" None (dispatch "/api/items/a1/other");
  Alcotest.(check (option reply)) "too short" None (dispatch "/api/items/a1");
  Alcotest.(check (option reply)) "too long" None (dispatch "/api/items/a1/tags/x");
  Alcotest.(check (option reply)) "method" None (dispatch ~meth:`POST "/api/items/a1/tags");
  (* No route matches a value with characters outside the allow-list. *)
  [ ".."; "."; "a%2Fb"; "a.b"; "a b" ]
  |> List.iter @@ fun value ->
     Alcotest.(check (option reply)) value None (dispatch ("/api/items/" ^ value ^ "/tags"))

let test_cases_are_tried_in_order_and_carry_endpoint_headers () =
  let headers : Ocapi.Endpoint.header list =
    [ { name = "X-A"; value = "a"; description = None } ]
  in
  let missing =
    Ocapi.Endpoint.Prism.make
      ~inject:(fun () -> `Missing)
      ~project:(function `Missing -> Some () | `Raw _ | `Unknown -> None)
  in
  let text =
    Ocapi.Endpoint.Prism.make
      ~inject:(fun (_, s) -> `Raw s)
      ~project:(function
        | `Raw s -> Some ([ "Content-Type", "text/plain" ], s) | `Missing | `Unknown -> None)
  in
  let responses =
    Ocapi.Endpoint.Response.
      [
        empty ~status:404 ~description:"missing" missing;
        (* Declaration order: a later case matching the same value is never used. *)
        empty ~status:410 ~description:"shadowed" missing;
        raw ~status:200 ~description:"raw" text;
      ]
  in
  let dispatch result =
    Ocapi.Server.dispatch
      [
        endpoint ~headers "/r" (Ocapi.Endpoint.Provide.return ()) responses
        |> Ocapi.Server.handle (fun () -> result);
      ]
      (request "/r")
  in
  Alcotest.(check (option reply))
    "first matching case, with the endpoint header"
    (Some { status = 404; headers = [ "X-A", "a" ]; body = Empty })
    (dispatch `Missing);
  Alcotest.(check (option reply))
    "raw"
    (Some { status = 200; headers = [ "Content-Type", "text/plain"; "X-A", "a" ]; body = Raw "x" })
    (dispatch (`Raw "x"));
  Alcotest.check_raises "no case" (Failure "no response case matches the result of /r") (fun () ->
    ignore (dispatch `Unknown))

(* A provider's failure carries the endpoint headers too. *)
let test_a_failing_provider_answers_with_the_endpoint_headers () =
  let headers : Ocapi.Endpoint.header list =
    [ { name = "X-A"; value = "a"; description = None } ]
  in
  let rejecting = Ocapi.Endpoint.Provide.make (fun _ -> Error (Ocapi.Endpoint.Reply.empty 400)) in
  Alcotest.(check (option reply))
    "400 with the header"
    (Some { status = 400; headers = [ "X-A", "a" ]; body = Empty })
    (Ocapi.Server.dispatch
       [ endpoint ~headers "/r" rejecting [ echo ] |> Ocapi.Server.handle Fun.id ]
       (request "/r"))

let test_serve_answers_not_found () =
  let routes =
    [ endpoint "/a" (Ocapi.Endpoint.Provide.return "a") [ echo ] |> Ocapi.Server.handle Fun.id ]
  in
  Alcotest.(check reply)
    "default" (Ocapi.Endpoint.Reply.empty 404)
    (Ocapi.Server.serve routes (request "/b"));
  Alcotest.(check reply)
    "custom"
    (Ocapi.Endpoint.Reply.json 404 (`String "nope"))
    (Ocapi.Server.serve
       ~not_found:(Ocapi.Endpoint.Reply.json 404 (`String "nope"))
       routes (request "/b"));
  Alcotest.(check reply)
    "matched"
    (Ocapi.Endpoint.Reply.json 200 (`String "a"))
    (Ocapi.Server.serve routes (request "/a"))

let () =
  Alcotest.run "server"
    [
      ( "dispatch",
        [
          Alcotest.test_case "matches the template" `Quick test_dispatch_matches_the_template;
          Alcotest.test_case "cases in order, endpoint headers" `Quick
            test_cases_are_tried_in_order_and_carry_endpoint_headers;
          Alcotest.test_case "a failing provider carries the endpoint headers" `Quick
            test_a_failing_provider_answers_with_the_endpoint_headers;
        ] );
      "serve", [ Alcotest.test_case "not found" `Quick test_serve_answers_not_found ];
    ]
