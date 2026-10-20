let echo =
  Ocapi.Endpoint.Response.json ~status:200 ~description:"echo"
    (Ocapi.Endpoint.Codec.make ~name:"echo" ~schema:(`Assoc [])
       (fun s -> `String s)
       Yojson.Safe.Util.to_string)
    Ocapi.Endpoint.Prism.id

let tag =
  Ocapi.Endpoint.make ~meth:`PATCH "/items/{id}/tags" ~tags:[] ~summary:"s"
    (Ocapi.Endpoint.Path.string "id" Fun.id)
    ~responses:[ echo ]

let list =
  Ocapi.Endpoint.make ~meth:`GET "/items" ~tags:[] ~summary:"s"
    (Ocapi.Endpoint.Provide.return "")
    ~responses:[ echo ]

let target =
  Alcotest.testable
    (fun fmt ({ meth; path } : Ocapi.Client.target) ->
      Format.fprintf fmt "%s %s" (Ocapi.Endpoint.meth_to_string meth) path)
    ( = )

let test_target_takes_the_method_and_path () =
  Alcotest.check target "target" { meth = `GET; path = "/items" } (Ocapi.Client.target list)

let test_fill_fills_the_template () =
  Alcotest.(check (result target string))
    "filled"
    (Ok { meth = `PATCH; path = "/items/item-1_a/tags" })
    (Ocapi.Client.fill tag [ "id", "item-1_a" ])

(* A value the server rejects is not sent: "../" or "?" could address another endpoint. *)
let test_a_value_the_server_rejects_is_not_sent () =
  [ ".."; "a/b"; "a?x"; "a%2Fb"; "" ]
  |> List.iter @@ fun value ->
     Alcotest.(check bool) value true (Result.is_error (Ocapi.Client.fill tag [ "id", value ]))

let test_a_missing_value_is_a_bug () =
  match Ocapi.Client.target tag with
  | _ -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let test_request_carries_bearer_and_json_body () =
  let request =
    Ocapi.Client.request ~bearer:"t0ken" ~body:"{}"
      ~query:[ "a", [ "1" ] ]
      (Ocapi.Client.target list)
  in
  Alcotest.(check (list (pair string string)))
    "headers"
    [ "authorization", "Bearer t0ken"; "content-type", "application/json" ]
    request.headers;
  Alcotest.(check (option string)) "body" (Some "{}") request.body;
  Alcotest.(check (list (pair string (list string)))) "query" [ "a", [ "1" ] ] request.query;
  Alcotest.(check (list (pair string string)))
    "no headers without them" [] (Ocapi.Client.request (Ocapi.Client.target list)).headers

let error =
  Ocapi.Endpoint.Codec.make ~name:"error" ~schema:(`Assoc [])
    (fun e -> `Assoc [ "error", `String e ])
    Yojson.Safe.Util.(fun json -> member "error" json |> to_string)

let get =
  Ocapi.Endpoint.make ~meth:`GET "/items/{id}" ~tags:[] ~summary:"s"
    (Ocapi.Endpoint.Path.string "id" Fun.id)
    ~responses:
      Ocapi.Endpoint.Response.
        [
          json ~status:200 ~description:"item"
            (Ocapi.Endpoint.Codec.make ~name:"item" ~schema:(`Assoc [])
               (fun s -> `String s)
               Yojson.Safe.Util.to_string)
            Ocapi.Endpoint.Prism.ok;
          json ~status:404 ~description:"missing" error Ocapi.Endpoint.Prism.error;
          outside ~status:500 ~description:"internal";
        ]

(* Answers [Send] with [status] and [body], recording the request. *)
let answering ?(headers = []) status body f =
  let seen = ref None in
  let result =
    try f ()
    with effect Ocapi.Client.Send request, k ->
      seen := Some request;
      Effect.Deep.continue k ({ status; headers; body } : Ocapi.Client.response)
  in
  result, !seen

let show = function
  | Ok (Ok item) -> "item " ^ item
  | Ok (Error e) -> "error " ^ e
  | Error (`Invalid _) -> "invalid"
  | Error `Unauthorized -> "unauthorized"
  | Error (`Undeclared (r : Ocapi.Client.response)) -> "undeclared " ^ string_of_int r.status
  | Error (`Malformed ((r : Ocapi.Client.response), _)) -> "malformed " ^ string_of_int r.status

(* The response becomes the handler's result again, through the case of its status. *)
let test_call_decodes_with_the_cases () =
  let check name expected status body =
    let result, _ = answering status body (fun () -> Ocapi.Client.call get "a1") in
    Alcotest.(check string) name expected (show result)
  in
  check "ok" "item x" 200 {|"x"|};
  check "a declared error" "error gone" 404 {|{"error":"gone"}|};
  check "401 without a case" "unauthorized" 401 "";
  check "a status from outside the endpoint" "undeclared 500" 500 "";
  check "an undocumented status" "undeclared 418" 418 "";
  check "a body the codec rejects" "malformed 200" 200 "{"

let test_call_sends_the_request () =
  let result, seen = answering 200 {|"x"|} (fun () -> Ocapi.Client.call ~bearer:"t" get "a1") in
  Alcotest.(check string) "result" "item x" (show result);
  match seen with
  | Some request ->
    Alcotest.(check string) "path" "/items/a1" request.path;
    Alcotest.(check (option string))
      "bearer, looked up case-insensitively" (Some "Bearer t")
      (Ocapi.Client.header "AUTHORIZATION" request.headers)
  | None -> Alcotest.fail "Send was not performed"

let test_an_invalid_value_is_not_sent () =
  let result, seen = answering 200 {|"x"|} (fun () -> Ocapi.Client.call get "../x") in
  Alcotest.(check string) "invalid" "invalid" (show result);
  Alcotest.(check bool) "nothing sent" true (Option.is_none seen)

(* A value the server's [Value] would not parse back is not sent: it would come back as a 404 or a
   400 the contract does not declare. *)
let test_a_value_the_server_would_reject_is_not_sent () =
  let page =
    Ocapi.Endpoint.make ~meth:`GET "/pages/{n}" ~tags:[] ~summary:"s"
      (let open Ocapi.Endpoint in
       let open Syntax in
       let+ n = Path.value (Value.int ~minimum:1 ()) "n" fst
       and+ size =
         Query.required ~description:"" (Value.int ~maximum:100 ()) "size" snd
         |> Provide.require (fun _ -> Reply.empty 400)
       in
       n, size)
      ~responses:[ echo ]
  in
  [ 0, 10; 1, 500 ]
  |> List.iter @@ fun request ->
     let result, seen = answering 200 {|"x"|} (fun () -> Ocapi.Client.call page request) in
     Alcotest.(check bool)
       "invalid" true
       (match result with Error (`Invalid _) -> true | Ok _ | Error _ -> false);
     Alcotest.(check bool) "nothing sent" true (Option.is_none seen)

(* An endpoint whose request is a header value the client sends as is. *)
let echo_header name =
  Ocapi.Endpoint.make ~meth:`GET "/h" ~tags:[] ~summary:"s"
    (Ocapi.Endpoint.Provide.make
       ~encode:(fun value -> { Ocapi.Endpoint.Encoded.empty with headers = [ name, value ] })
       (fun r -> Ok (Option.value (r.header name) ~default:"")))
    ~responses:[ echo ]

(* A line break would let the value write headers of its own; nothing is sent. *)
let test_a_header_with_a_line_break_is_not_sent () =
  [
    (fun () -> Ocapi.Client.call (echo_header "x-token") "t\r\nX-Injected: 1");
    (fun () -> Ocapi.Client.call (echo_header "x-token") "t\nX-Injected: 1");
    (fun () -> Ocapi.Client.call ~bearer:"b\r\nX-Injected: 2" (echo_header "x-token") "t");
  ]
  |> List.iter @@ fun call ->
     let result, seen = answering 200 {|"x"|} call in
     Alcotest.(check bool)
       "invalid" true
       (match result with Error (`Invalid _) -> true | Ok _ | Error _ -> false);
     Alcotest.(check bool) "nothing sent" true (Option.is_none seen)

(* Two Authorization headers would let a proxy and the server authenticate different users. *)
let test_authorization_is_given_once () =
  let result, seen =
    answering 200 {|"x"|} (fun () ->
      Ocapi.Client.call ~bearer:"b" (echo_header "authorization") "Bearer r")
  in
  Alcotest.(check bool)
    "invalid" true
    (match result with Error (`Invalid _) -> true | Ok _ | Error _ -> false);
  Alcotest.(check bool) "nothing sent" true (Option.is_none seen)

type cookies = { a : string; b : string }

(* Cookies go out in one Cookie header (RFC 6265 §5.4). *)
let test_cookies_go_in_one_header () =
  let both =
    Ocapi.Endpoint.make ~meth:`GET "/c" ~tags:[] ~summary:"s"
      (let open Ocapi.Endpoint in
       let open Syntax in
       let+ a =
         Cookie.required ~description:"" Value.string "a" (fun r -> r.a)
         |> Provide.require (fun _ -> Reply.empty 400)
       and+ b =
         Cookie.required ~description:"" Value.string "b" (fun r -> r.b)
         |> Provide.require (fun _ -> Reply.empty 400)
       in
       { a; b })
      ~responses:[ echo ]
  in
  let _, seen = answering 200 {|"x"|} (fun () -> Ocapi.Client.call both { a = "1"; b = "2" }) in
  match seen with
  | Some request ->
    Alcotest.(check (list (pair string string)))
      "one header"
      [ "cookie", "a=1; b=2" ]
      (request.headers |> List.filter (fun (name, _) -> name = "cookie"))
  | None -> Alcotest.fail "Send was not performed"

let () =
  Alcotest.run "client"
    [
      ( "target",
        [
          Alcotest.test_case "takes the method and path" `Quick
            test_target_takes_the_method_and_path;
          Alcotest.test_case "fills the template" `Quick test_fill_fills_the_template;
          Alcotest.test_case "a value the server rejects is not sent" `Quick
            test_a_value_the_server_rejects_is_not_sent;
          Alcotest.test_case "a missing value is a bug" `Quick test_a_missing_value_is_a_bug;
        ] );
      ( "request",
        [
          Alcotest.test_case "bearer and JSON body" `Quick test_request_carries_bearer_and_json_body;
          Alcotest.test_case "call decodes with the cases" `Quick test_call_decodes_with_the_cases;
          Alcotest.test_case "call sends the request" `Quick test_call_sends_the_request;
          Alcotest.test_case "an invalid value is not sent" `Quick test_an_invalid_value_is_not_sent;
          Alcotest.test_case "a header with a line break is not sent" `Quick
            test_a_header_with_a_line_break_is_not_sent;
          Alcotest.test_case "authorization is given once" `Quick test_authorization_is_given_once;
          Alcotest.test_case "a value the server would reject is not sent" `Quick
            test_a_value_the_server_would_reject_is_not_sent;
          Alcotest.test_case "cookies go in one header" `Quick test_cookies_go_in_one_header;
        ] );
    ]
