open Ocapi

let echo =
  Endpoint.Response.json ~status:200 ~description:"echo"
    (Endpoint.Codec.make ~name:"echo" ~schema:(`Assoc [])
       (fun s -> `String s)
       Yojson.Safe.Util.to_string)
    Endpoint.Prism.id

type note = { id : string; text : string; auth : string option }

(* A header both ways: [Provide.make] with an [encode] of its own. *)
let authorization get =
  Endpoint.Provide.make
    ~encode:(fun q ->
      {
        Endpoint.Encoded.empty with
        headers = get q |> Option.fold ~none:[] ~some:(fun v -> [ "authorization", v ]);
      })
    (fun r -> Ok (r.header "authorization"))

let item =
  let open Endpoint.Syntax in
  Endpoint.make ~meth:`POST "/items/{id}" ~tags:[] ~summary:"s"
    (let+ id = Endpoint.Path.string "id" (fun r -> r.id)
     and+ text =
       Endpoint.Body.raw ~name:"b" ~schema:(`Assoc []) ~content_type:"text/plain" (fun r -> r.text)
     and+ auth = authorization (fun r -> r.auth) in
     { id; text; auth })
    ~responses:[ echo ]

let routes =
  [
    item
    |> Server.handle (fun { id; text; auth } ->
      String.concat " " [ id; text; Option.value auth ~default:"anonymous" ]);
  ]

let test_the_client_reaches_the_handler () =
  In_process.run routes @@ fun () ->
  (match Client.call item { id = "a1"; text = "hi"; auth = Some "t" } with
  | Ok echoed -> Alcotest.(check string) "decoded" "a1 hi t" echoed
  | Error _ -> Alcotest.fail "expected Ok");
  let response =
    Client.fill item [ "id", "a1" ] |> Result.get_ok |> Client.request ~body:"" |> Client.send
  in
  Alcotest.(check (option string))
    "content type" (Some "application/json")
    (Client.header "Content-Type" response.headers)

let test_no_route_is_a_failure () =
  In_process.run routes @@ fun () ->
  Alcotest.(check int)
    "404" 404 (Client.request { meth = `GET; path = "/nowhere" } |> Client.send).status

let () =
  Alcotest.run "in_process"
    [
      ( "run",
        [
          Alcotest.test_case "the client reaches the handler" `Quick
            test_the_client_reaches_the_handler;
          Alcotest.test_case "no route is a failure" `Quick test_no_route_is_a_failure;
        ] );
    ]
