open Ocapi.Endpoint.Syntax

let request ?(meth = `GET) ?(query = []) ?(body = "") path : Ocapi.Endpoint.Request.t =
  {
    meth;
    segments = String.split_on_char '/' path |> List.filter (fun s -> s <> "");
    query;
    header = (fun _ -> None);
    body = (fun () -> body);
    params = [];
  }

let string_schema = `Assoc [ "type", `String "string" ]

let string_codec =
  Ocapi.Endpoint.Codec.make ~name:"b" ~schema:string_schema
    (fun s -> `String s)
    Yojson.Safe.Util.to_string

let bad_request name = Ocapi.Endpoint.Reply.json 400 (`Assoc [ "bad", `String name ])

(* Records the names in resolution order; names in [failing] fail. *)
let traced ~trace ~failing name =
  Ocapi.Endpoint.Provide.make @@ fun _ ->
  trace := name :: !trace;
  if List.mem name failing then Error (bad_request name) else Ok name

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
    (Ocapi.Endpoint.Codec.make ~name:"echo" ~schema:string_schema
       (fun s -> `String s)
       Yojson.Safe.Util.to_string)
    Ocapi.Endpoint.Prism.id

let endpoint ?(meth = `GET) ?headers ?hidden template provide responses =
  Ocapi.Endpoint.make ~meth template ?headers ?hidden ~tags:[] ~summary:"s" provide ~responses

let run ?(meth = `GET) endpoint ?query ?body f path =
  Ocapi.Server.dispatch [ endpoint |> Ocapi.Server.handle f ] (request ~meth ?query ?body path)

(* Left to right, stopping at the first failure: the providers after it (reading the body, say)
   do not run. *)
let test_providers_resolve_left_to_right () =
  let trace = ref [] in
  let check name expected failing =
    trace := [];
    let provide =
      let+ a = traced ~trace ~failing "a"
      and+ b = traced ~trace ~failing "b"
      and+ c = traced ~trace ~failing "c" in
      a ^ b ^ c
    in
    let result = run (endpoint "/x" provide [ echo ]) Fun.id "/x" in
    Alcotest.(check (pair (option reply) (list string))) name expected (result, List.rev !trace)
  in
  check "all" (Some (Ocapi.Endpoint.Reply.json 200 (`String "abc")), [ "a"; "b"; "c" ]) [];
  check "stops at the first failure" (Some (bad_request "b"), [ "a"; "b" ]) [ "b"; "c" ]

let decode provide query =
  match run (endpoint "/q" provide [ echo ]) ~query Fun.id "/q" with
  | Some { body = Json (`String s); _ } -> s
  | Some _ | None -> Alcotest.fail "expected a JSON string"

(* As [Uri.get_query_param]: the values of the first key, joined with ','. *)
let test_query_reads_like_uri () =
  let check name expected ?unique query =
    let provide =
      let+ r =
        Ocapi.Endpoint.Query.required ?unique ~description:"k" Ocapi.Endpoint.Value.string "k"
          Fun.id
      in
      match r with Ok s -> "ok " ^ s | Error `Missing -> "missing" | Error `Invalid -> "invalid"
    in
    Alcotest.(check string) name expected (decode provide query)
  in
  check "single" "ok 1" [ "k", [ "1" ] ];
  check "values joined" "ok 1,2" [ "k", [ "1"; "2" ] ];
  check "first key" "ok 1" [ "k", [ "1" ]; "k", [ "2" ] ];
  check "without a value" "ok " [ "k", [] ];
  check "absent" "missing" [];
  check "unique rejects a repeated key" "invalid" ~unique:true [ "k", [ "1" ]; "k", [ "1" ] ]

let test_optional_and_int () =
  let provide =
    let+ r =
      Ocapi.Endpoint.Query.optional ~description:"n"
        (Ocapi.Endpoint.Value.int ~minimum:1 ~maximum:10 ())
        "n" int_of_string_opt
    in
    match r with
    | Ok None -> "none"
    | Ok (Some n) -> string_of_int n
    | Error `Missing -> "missing"
    | Error `Invalid -> "invalid"
  in
  let check name expected query = Alcotest.(check string) name expected (decode provide query) in
  check "absent" "none" [];
  check "in range" "10" [ "n", [ "10" ] ];
  check "below" "invalid" [ "n", [ "0" ] ];
  check "above" "invalid" [ "n", [ "11" ] ];
  check "not an int" "invalid" [ "n", [ "x" ] ];
  check "hexadecimal" "invalid" [ "n", [ "0x5" ] ];
  check "underscore" "invalid" [ "n", [ "1_0" ] ];
  check "plus sign" "invalid" [ "n", [ "+5" ] ];
  Alcotest.(check string)
    "schema" {|{"type":"integer","minimum":1,"maximum":10}|}
    (Yojson.Safe.to_string (Ocapi.Endpoint.Value.int_schema ~minimum:1 ~maximum:10 ()))

let test_require_turns_a_failure_into_a_reply () =
  let provide =
    Ocapi.Endpoint.Provide.require
      (function `Missing -> bad_request "missing" | `Invalid -> bad_request "invalid")
      (Ocapi.Endpoint.Query.required ~description:"k" Ocapi.Endpoint.Value.string "k" Fun.id)
  in
  Alcotest.(check (option reply))
    "missing"
    (Some (bad_request "missing"))
    (run (endpoint "/q" provide [ echo ]) Fun.id "/q")

let test_body () =
  let provide =
    let+ r = Ocapi.Endpoint.Body.json string_codec Fun.id in
    match r with Ok s -> s | Error (`Malformed _) -> "malformed"
  in
  let post body = run ~meth:`POST (endpoint ~meth:`POST "/b" provide [ echo ]) ~body Fun.id "/b" in
  let json s = Some (Ocapi.Endpoint.Reply.json 200 (`String s)) in
  Alcotest.(check (option reply)) "ok" (json "x") (post {|"x"|});
  Alcotest.(check (option reply)) "not JSON" (json "malformed") (post "{");
  Alcotest.(check (option reply)) "of_json raised" (json "malformed") (post "1")

let test_template_must_match_the_path_parameters () =
  Alcotest.check_raises "mismatch"
    (Invalid_argument "path parameters of /x/{id} do not match its template") (fun () ->
    ignore (endpoint "/x/{id}" (Ocapi.Endpoint.Path.string "other" Fun.id) [ echo ]))

(* Reading the body twice does not compile; see [Provide.t]. *)
let test_at_least_one_response () =
  Alcotest.check_raises "no response" (Invalid_argument "/r declares no response") (fun () ->
    ignore (endpoint "/r" (Ocapi.Endpoint.Provide.return "") []))

(* Reading the body is the adapter's; what it raises is not a malformed body. *)
let test_a_failure_to_read_the_body_propagates () =
  let provide =
    let+ r = Ocapi.Endpoint.Body.json string_codec Fun.id in
    match r with Ok s -> s | Error (`Malformed _) -> "malformed"
  in
  let request = { (request ~meth:`POST "/b") with body = (fun () -> failwith "limit") } in
  Alcotest.check_raises "propagates" (Failure "limit") (fun () ->
    ignore (Ocapi.Endpoint.resolve (endpoint ~meth:`POST "/b" provide [ echo ]) request))

(* Path parameters first, query parameters in provider order; schemas of one status as a oneOf. *)
let test_openapi_operation () =
  let other =
    Ocapi.Endpoint.Codec.make ~name:"other_thing" ~schema:string_schema
      (fun s -> `String s)
      Yojson.Safe.Util.to_string
  in
  let documented =
    Ocapi.Endpoint.make ~meth:`PATCH "/api/items/{id}" ~tags:[ "Item" ] ~summary:"s"
      ~headers:[ { name = "Cache-Control"; value = "no-store"; description = Some "d" } ]
      (let+ _ =
         Ocapi.Endpoint.Query.required ~description:"q" Ocapi.Endpoint.Value.string "q" Fun.id
       and+ id = Ocapi.Endpoint.Path.string ~description:"item id" "id" Fun.id
       and+ _ =
         Ocapi.Endpoint.Body.raw ~name:"item_request" ~schema:string_schema
           ~content_type:"application/json" Fun.id
       in
       id)
      ~responses:
        Ocapi.Endpoint.Response.
          [
            outside ~status:500 ~description:"internal";
            echo;
            json ~status:200 ~description:"ignored" other Ocapi.Endpoint.Prism.id;
          ]
  in
  Alcotest.(check string)
    "operation"
    ({|{"tags":["Item"],"summary":"s",|}
   ^ {|"requestBody":{"required":true,"content":{"application/json":{"schema":{"$ref":"#/components/schemas/ItemRequest"}}}},|}
   ^ {|"parameters":[{"name":"id","in":"path","required":true,"description":"item id","schema":{"type":"string"}},|}
   ^ {|{"name":"q","in":"query","required":true,"description":"q","schema":{"type":"string"}}],|}
   ^ {|"responses":{"200":{"description":"echo","headers":{"Cache-Control":{"description":"d","schema":{"type":"string"}}},|}
   ^ {|"content":{"application/json":{"schema":{"oneOf":[{"$ref":"#/components/schemas/Echo"},{"$ref":"#/components/schemas/OtherThing"}]}}}},|}
   ^ {|"500":{"description":"internal"}}}|})
    (Yojson.Safe.to_string (Ocapi.Endpoint.Openapi.operation documented))

let test_openapi_paths () =
  let get = endpoint "/a" (Ocapi.Endpoint.Provide.return "") [ echo ] in
  let post = endpoint ~meth:`POST "/a" (Ocapi.Endpoint.Provide.return "") [ echo ] in
  let hidden = endpoint ~hidden:true "/h" (Ocapi.Endpoint.Provide.return "") [ echo ] in
  Alcotest.(check (list (pair string (list string))))
    "grouped by template, hidden left out"
    [ "/a", [ "get"; "post" ] ]
    (Ocapi.Endpoint.Openapi.(paths [ get; post; hidden ])
    |> List.map @@ fun (t, item) ->
       ( t,
         match item with
         | `Assoc ops -> List.map fst ops
         | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> [] ))

(* Endpoints of different types share a list literal; a binding opens [Endpoints] locally. *)
let test_endpoints_of_different_types () =
  let text = endpoint "/text" (Ocapi.Endpoint.Provide.return "") [ echo ] in
  let count =
    endpoint "/count" (Ocapi.Endpoint.Provide.return 1)
      [
        Ocapi.Endpoint.Response.json ~status:200 ~description:"count"
          (Ocapi.Endpoint.Codec.make ~name:"count" ~schema:(`Assoc [])
             (fun n -> `Int n)
             Yojson.Safe.Util.to_int)
          Ocapi.Endpoint.Prism.id;
      ]
  in
  let first = Ocapi.Endpoint.Openapi.Endpoints.[ text ] in
  Alcotest.(check (list string))
    "templates in order" [ "/text"; "/count" ]
    (Ocapi.Endpoint.Openapi.(paths (Endpoints.append first [ count ])) |> List.map fst)

(* In order of appearance, without duplicates, hidden endpoints left out; one name with two
   schemas is a bug. *)
let test_openapi_schemas () =
  let int_schema = `Assoc [ "type", `String "integer" ] in
  let body name schema = Ocapi.Endpoint.Body.raw ~name ~schema ~content_type:"text/plain" Fun.id in
  let a = endpoint ~meth:`POST "/a" (body "request" int_schema) [ echo ] in
  let b = endpoint ~meth:`POST "/b" (body "request" int_schema) [ echo ] in
  let hidden = endpoint ~hidden:true ~meth:`POST "/h" (body "secret" int_schema) [ echo ] in
  Alcotest.(check (list string))
    "names" [ "request"; "echo" ]
    (Ocapi.Endpoint.Openapi.(schemas [ a; b; hidden ]) |> List.map fst);
  let conflicting = endpoint ~meth:`POST "/c" (body "request" string_schema) [ echo ] in
  Alcotest.check_raises "conflict"
    (Invalid_argument "schema request is declared with two different schemas") (fun () ->
    ignore Ocapi.Endpoint.Openapi.(schemas [ a; conflicting ]))

(* One status, described by the most specific source: the cases, the providers, the transport.
   The endpoint headers go with what the endpoint answers, not with what the transport does. *)
let test_responses_from_providers_and_transport () =
  let common : Ocapi.Endpoint.Openapi.common list =
    [
      { status = 400; description = "transport 400"; scope = `Every_operation };
      { status = 413; description = "too large"; scope = `Operations_with_body };
      { status = 500; description = "internal"; scope = `Every_operation };
    ]
  in
  let rejecting =
    Ocapi.Endpoint.Provide.make
      ~responses:[ 400, "provider 400"; 422, "provider 422" ]
      (fun _ -> Ok "")
  in
  let headers : Ocapi.Endpoint.header list =
    [ { name = "X-A"; value = "a"; description = Some "d" } ]
  in
  let statuses endpoint =
    match
      Ocapi.Endpoint.Openapi.operation ~common endpoint |> Yojson.Safe.Util.member "responses"
    with
    | `Assoc responses ->
      responses
      |> List.map @@ fun (status, response) ->
         ( status,
           Yojson.Safe.Util.(member "description" response |> to_string),
           Yojson.Safe.Util.member "headers" response <> `Null )
    | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> []
  in
  Alcotest.(check (list (triple string string bool)))
    "without a body"
    [
      "200", "echo", true;
      "400", "provider 400", true;
      "422", "provider 422", true;
      "500", "internal", false;
    ]
    (statuses (endpoint ~headers "/a" rejecting [ echo ]));
  Alcotest.(check (list (triple string string bool)))
    "from outside the endpoint"
    [
      "200", "echo", true;
      "400", "provider 400", true;
      "422", "provider 422", true;
      "500", "outside", false;
    ]
    (statuses
       (endpoint ~headers "/a" rejecting
          Ocapi.Endpoint.Response.
            [
              echo;
              outside ~status:200 ~description:"outside";
              outside ~status:500 ~description:"outside";
            ]));
  let with_body =
    let+ _ = rejecting
    and+ body =
      Ocapi.Endpoint.Body.raw ~name:"b" ~schema:string_schema ~content_type:"text/plain" Fun.id
    in
    body
  in
  Alcotest.(check (list string))
    "with a body"
    [ "200"; "400"; "413"; "422"; "500" ]
    (statuses (endpoint ~meth:`POST "/b" with_body [ echo ]) |> List.map (fun (s, _, _) -> s));
  let required =
    Ocapi.Endpoint.Query.required ~description:"k" Ocapi.Endpoint.Value.string "k" Fun.id
    |> Ocapi.Endpoint.Provide.require
         ~responses:[ 400, "k is missing" ]
         (fun _ -> Ocapi.Endpoint.Reply.empty 400)
  in
  Alcotest.(check (list (triple string string bool)))
    "require declares its reply"
    [ "200", "echo", false; "400", "k is missing", false; "500", "internal", false ]
    (statuses (endpoint "/q" required [ echo ]))

(* What the server replies, the client decodes back into the same result: through each kind of
   case, errors routed by status included. *)
let test_a_reply_decodes_back () =
  let error =
    Ocapi.Endpoint.Codec.make ~name:"error" ~schema:(`Assoc [])
      (fun (code, message) -> `Assoc [ "code", `Int code; "message", `String message ])
      Yojson.Safe.Util.(
        fun json -> member "code" json |> to_int, member "message" json |> to_string)
  in
  let responses =
    Ocapi.Endpoint.Response.(
      [
        json ~status:200 ~description:""
          (Ocapi.Endpoint.Codec.make ~name:"n" ~schema:(`Assoc [])
             (fun n -> `Int n)
             Yojson.Safe.Util.to_int)
          (Ocapi.Endpoint.Prism.make
             ~inject:(fun n -> `Count n)
             ~project:(function `Count n -> Some n | `Gone | `Text _ | `Failed _ -> None));
        empty ~status:204 ~description:""
          (Ocapi.Endpoint.Prism.make
             ~inject:(fun () -> `Gone)
             ~project:(function `Gone -> Some () | `Count _ | `Text _ | `Failed _ -> None));
        raw ~status:202 ~description:""
          (Ocapi.Endpoint.Prism.make
             ~inject:(fun (_, s) -> `Text s)
             ~project:(function `Text s -> Some ([], s) | `Count _ | `Gone | `Failed _ -> None));
      ]
      @ errors error ~status:fst
          (Ocapi.Endpoint.Prism.make
             ~inject:(fun e -> `Failed e)
             ~project:(function `Failed e -> Some e | `Count _ | `Gone | `Text _ -> None))
          [ 404, "missing"; 409, "conflict" ])
  in
  let e = endpoint "/r" (Ocapi.Endpoint.Provide.return ()) responses in
  let round_trip r =
    let reply = Ocapi.Endpoint.reply e r in
    let body =
      match reply.body with Empty -> "" | Raw s -> s | Json j -> Yojson.Safe.to_string j
    in
    Ocapi.Endpoint.decode e ~status:reply.status ~headers:reply.headers body
  in
  [ `Count 3; `Gone; `Text "hi"; `Failed (404, "no"); `Failed (409, "taken") ]
  |> List.iter (fun r -> Alcotest.(check bool) "round trip" true (round_trip r = Ok r));
  Alcotest.(check bool)
    "an error body under another status's case does not decode" true
    (Ocapi.Endpoint.decode e ~status:404 ~headers:[] {|{"code":409,"message":"x"}|}
    = Error `Undeclared)

type note = { id : string; limit : int option; tag : string; text : string }

(* What the client writes for a request, the server reads back into the same record. *)
let test_a_request_encodes_back () =
  let open Ocapi.Endpoint in
  let endpoint =
    make ~meth:`POST "/notes/{id}" ~tags:[] ~summary:"s"
      (let+ id = Path.string "id" (fun r -> r.id)
       and+ limit =
         Query.optional ~description:"" (Value.int ()) "limit" (fun r -> r.limit)
         |> Provide.require (fun _ -> Reply.empty 400)
       and+ tag =
         Query.required ~description:"" Value.string "tag" (fun r -> r.tag)
         |> Provide.require (fun _ -> Reply.empty 400)
       and+ text =
         Body.json string_codec (fun r -> r.text) |> Provide.require (fun _ -> Reply.empty 400)
       in
       { id; limit; tag; text })
      ~responses:[ echo ]
  in
  let round_trip note =
    let encoded = encode endpoint note |> Result.get_ok in
    let target = Ocapi.Client.fill endpoint encoded.params |> Result.get_ok in
    let request =
      {
        (request ~meth:`POST ~query:encoded.query ~body:(Option.get encoded.body) target.path) with
        params = [];
      }
    in
    match params endpoint request.segments with
    | Some params -> resolve endpoint { request with params }
    | None -> Alcotest.fail "the encoded path does not match the template"
  in
  [
    { id = "a1"; limit = Some 3; tag = "x,y"; text = "hello" };
    { id = "b-2"; limit = None; tag = ""; text = "" };
  ]
  |> List.iter @@ fun note -> Alcotest.(check bool) note.id true (round_trip note = Ok note)

(* An empty case of a status leaves a body to the other cases of that status. *)
let test_an_empty_case_does_not_take_a_body () =
  let e =
    endpoint "/r"
      (Ocapi.Endpoint.Provide.return ())
      Ocapi.Endpoint.Response.
        [
          empty ~status:200 ~description:"" Ocapi.Endpoint.Prism.none;
          json ~status:200 ~description:"" string_codec Ocapi.Endpoint.Prism.some;
        ]
  in
  let decode body = Ocapi.Endpoint.decode e ~status:200 ~headers:[] body in
  Alcotest.(check bool) "a body is the json case" true (decode {|"payload"|} = Ok (Some "payload"));
  Alcotest.(check bool) "no body is the empty case" true (decode "" = Ok None)

let test_templates_are_checked () =
  Alcotest.check_raises "no leading slash"
    (Invalid_argument "the path template items does not start with /") (fun () ->
    ignore (endpoint "items" (Ocapi.Endpoint.Provide.return "") [ echo ]));
  Alcotest.check_raises "a parameter twice"
    (Invalid_argument "the path template /a/{x}/{x} names a parameter twice") (fun () ->
    ignore
      (endpoint "/a/{x}/{x}"
         (let+ x = Ocapi.Endpoint.Path.string "x" Fun.id
          and+ _ = Ocapi.Endpoint.Path.string "x" Fun.id in
          x)
         [ echo ]))

(* The document refuses what OpenAPI forbids rather than emitting it. *)
let test_the_document_refuses_duplicates () =
  let at template = endpoint template (Ocapi.Endpoint.Path.string "x" Fun.id) [ echo ] in
  let a = endpoint "/a" (Ocapi.Endpoint.Provide.return "") [ echo ] in
  let post = endpoint ~meth:`POST "/a" (Ocapi.Endpoint.Provide.return "") [ echo ] in
  Alcotest.check_raises "one method twice" (Invalid_argument "GET /a is declared twice") (fun () ->
    ignore Ocapi.Endpoint.Openapi.(paths [ a; post; a ]));
  Alcotest.check_raises "paths differing in parameter names only"
    (Invalid_argument "paths /b/{x} and /b/{y} are the same path") (fun () ->
    ignore
      Ocapi.Endpoint.Openapi.(
        paths [ at "/b/{x}"; endpoint "/b/{y}" (Ocapi.Endpoint.Path.string "y" Fun.id) [ echo ] ]));
  let codec name =
    Ocapi.Endpoint.Codec.make ~name
      ~schema:(`Assoc [ "type", `String name ])
      (fun s -> `String s)
      Yojson.Safe.Util.to_string
  in
  let body name =
    endpoint ~meth:`POST ("/" ^ name)
      (Ocapi.Endpoint.Body.json (codec name) Fun.id
      |> Ocapi.Endpoint.Provide.require (fun _ -> Ocapi.Endpoint.Reply.empty 400))
      [ echo ]
  in
  Alcotest.check_raises "one components name"
    (Invalid_argument
       "schemas foo_bar and foo__bar are both FooBar in the components, with different schemas")
    (fun () -> ignore Ocapi.Endpoint.Openapi.(schemas [ body "foo_bar"; body "foo__bar" ]))

(* A raw body travels and is documented under its own media type, not JSON. *)
let test_a_raw_body_has_its_content_type () =
  let csv =
    endpoint ~meth:`POST "/csv"
      (Ocapi.Endpoint.Body.raw ~name:"table" ~schema:string_schema ~content_type:"text/csv" Fun.id)
      [ echo ]
  in
  Alcotest.(check (option string))
    "sent" (Some "text/csv")
    ( Ocapi.Endpoint.encode csv "a,b" |> Result.get_ok |> fun encoded ->
      Ocapi.Client.header "content-type" encoded.headers );
  Alcotest.(check (list string))
    "documented" [ "text/csv" ]
    (match
       Ocapi.Endpoint.Openapi.operation csv
       |> Yojson.Safe.Util.member "requestBody"
       |> Yojson.Safe.Util.member "content"
     with
    | `Assoc media -> List.map fst media
    | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> [])

type traced = { trace : string; retries : int option }

(* A header both ways: written by the client, read back by the server, documented as a header. *)
let test_header_parameters () =
  let open Ocapi.Endpoint in
  let traced =
    make ~meth:`GET "/t" ~tags:[] ~summary:"s"
      (let+ trace =
         Header.required ~description:"" Value.string "X-Trace" (fun r -> r.trace)
         |> Provide.require (fun _ -> Reply.empty 400)
       and+ retries =
         Header.optional ~description:"" (Value.int ()) "X-Retries" (fun r -> r.retries)
         |> Provide.require (fun _ -> Reply.empty 400)
       in
       { trace; retries })
      ~responses:[ echo ]
  in
  let round_trip q =
    let headers = (encode traced q |> Result.get_ok).headers in
    resolve traced
      {
        (request "/t") with
        header =
          (fun name ->
            headers
            |> List.find_map (fun (key, value) ->
              if String.lowercase_ascii key = String.lowercase_ascii name then Some value else None));
      }
  in
  [ { trace = "abc"; retries = Some 2 }; { trace = ""; retries = None } ]
  |> List.iter (fun q -> Alcotest.(check bool) q.trace true (round_trip q = Ok q));
  Alcotest.(check (list string))
    "documented as headers" [ "header"; "header" ]
    (match Openapi.operation traced |> Yojson.Safe.Util.member "parameters" with
    | `List parameters ->
      parameters |> List.map (fun p -> Yojson.Safe.Util.(member "in" p |> to_string))
    | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ -> []);
  Alcotest.check_raises "Authorization is security"
    (Invalid_argument "the header Authorization is not a parameter; declare it as security or media")
    (fun () -> ignore (Header.required ~description:"" Value.string "Authorization" Fun.id))

(* A rejection sent with a codec is documented with that codec's schema. *)
let test_a_json_rejection_is_documented () =
  let error =
    Ocapi.Endpoint.Codec.make ~name:"error"
      ~schema:(`Assoc [ "type", `String "object" ])
      (fun e -> `Assoc [ "error", `String e ])
      Yojson.Safe.Util.(fun json -> member "error" json |> to_string)
  in
  let e =
    endpoint "/q"
      (Ocapi.Endpoint.Query.required ~description:"" Ocapi.Endpoint.Value.string "k" Fun.id
      |> Ocapi.Endpoint.Provide.require_json ~status:422 ~description:"k is missing" error (fun _ ->
        "missing_k"))
      [ echo ]
  in
  Alcotest.(check bool)
    "sent" true
    (Ocapi.Endpoint.resolve e (request "/q")
    = Error (Ocapi.Endpoint.Reply.json 422 (`Assoc [ "error", `String "missing_k" ])));
  Alcotest.(check string)
    "documented"
    {|{"description":"k is missing","content":{"application/json":{"schema":{"$ref":"#/components/schemas/Error"}}}}|}
    (Ocapi.Endpoint.Openapi.operation e
    |> Yojson.Safe.Util.member "responses"
    |> Yojson.Safe.Util.member "422" |> Yojson.Safe.to_string);
  Alcotest.(check (list string))
    "in the components" [ "error"; "echo" ]
    (Ocapi.Endpoint.Openapi.schemas [ e ] |> List.map fst)

(* Headers chosen per response travel with the value and come back to the client. *)
let test_json_with_headers () =
  let e =
    endpoint "/login"
      (Ocapi.Endpoint.Provide.return ())
      [
        Ocapi.Endpoint.Response.json_with_headers ~status:200 ~description:"signed in"
          ~headers:[ "Set-Cookie", "the session" ]
          string_codec Ocapi.Endpoint.Prism.id;
      ]
  in
  let reply = Ocapi.Endpoint.reply e ([ "set-cookie", "s=1" ], "alice") in
  Alcotest.(check (list (pair string string))) "sent" [ "set-cookie", "s=1" ] reply.headers;
  Alcotest.(check bool)
    "received" true
    (Ocapi.Endpoint.decode e ~status:200 ~headers:reply.headers {|"alice"|}
    = Ok ([ "set-cookie", "s=1" ], "alice"));
  Alcotest.(check string)
    "documented" {|{"Set-Cookie":{"description":"the session","schema":{"type":"string"}}}|}
    (Ocapi.Endpoint.Openapi.operation e
    |> Yojson.Safe.Util.(fun o -> member "responses" o |> member "200" |> member "headers")
    |> Yojson.Safe.to_string)

type session = { session : string; theme : string option }

(* Cookies are read by name from the Cookie header and documented as cookie parameters. *)
let test_cookie_parameters () =
  let open Ocapi.Endpoint in
  let e =
    make ~meth:`GET "/me" ~tags:[] ~summary:"s"
      (let+ session =
         Cookie.required ~description:"" Value.string "session" (fun r -> r.session)
         |> Provide.require (fun _ -> Reply.empty 401)
       and+ theme =
         Cookie.optional ~description:"" Value.string "theme" (fun r -> r.theme)
         |> Provide.require (fun _ -> Reply.empty 400)
       in
       { session; theme })
      ~responses:[ echo ]
  in
  let with_cookie value =
    { (request "/me") with header = (fun name -> if name = "cookie" then Some value else None) }
  in
  Alcotest.(check bool)
    "both" true
    (resolve e (with_cookie "theme=dark; session=abc") = Ok { session = "abc"; theme = Some "dark" });
  Alcotest.(check bool)
    "without the optional one" true
    (resolve e (with_cookie "session=abc") = Ok { session = "abc"; theme = None });
  Alcotest.(check bool)
    "missing" true
    (resolve e (with_cookie "theme=dark") = Error (Reply.empty 401));
  Alcotest.(check (list string))
    "documented" [ "cookie"; "cookie" ]
    (match Openapi.operation e |> Yojson.Safe.Util.member "parameters" with
    | `List ps -> ps |> List.map (fun p -> Yojson.Safe.Util.(member "in" p |> to_string))
    | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ -> []);
  Alcotest.(check bool)
    "a value a cookie cannot hold is not encoded" true
    (Result.is_error (encode e { session = "a b"; theme = None }))

type page = { offset : int; size : int }
type search = { text : string; page : page }

(* A provider written once against its own record, used as a part of a larger one. *)
let test_contramap () =
  let open Ocapi.Endpoint in
  let paging =
    let+ offset =
      Query.required ~description:"" (Value.int ()) "offset" (fun p -> p.offset)
      |> Provide.require (fun _ -> Reply.empty 400)
    and+ size =
      Query.required ~description:"" (Value.int ()) "size" (fun p -> p.size)
      |> Provide.require (fun _ -> Reply.empty 400)
    in
    { offset; size }
  in
  let search =
    make ~meth:`GET "/search" ~tags:[] ~summary:"s"
      (let+ text =
         Query.required ~description:"" Value.string "q" (fun r -> r.text)
         |> Provide.require (fun _ -> Reply.empty 400)
       and+ page = Provide.contramap (fun r -> r.page) paging in
       { text; page })
      ~responses:[ echo ]
  in
  let q = { text = "ocaml"; page = { offset = 20; size = 10 } } in
  let encoded = encode search q |> Result.get_ok in
  Alcotest.(check (list (pair string (list string))))
    "written"
    [ "q", [ "ocaml" ]; "offset", [ "20" ]; "size", [ "10" ] ]
    encoded.query;
  Alcotest.(check bool)
    "read back" true
    (resolve search (request ~query:encoded.query "/search") = Ok q)

let test_bool () =
  let parse s =
    Ocapi.Endpoint.Provide.resolve
      (Ocapi.Endpoint.Query.required ~description:"" Ocapi.Endpoint.Value.bool "b" Fun.id)
      (request ~query:[ "b", [ s ] ] "/")
  in
  Alcotest.(check bool) "true" true (parse "true" = Ok (Ok true));
  Alcotest.(check bool) "false" true (parse "false" = Ok (Ok false));
  Alcotest.(check bool) "anything else" true (parse "1" = Ok (Error `Invalid))

(* Schemes of one provider are alternatives; every provider must be satisfied. *)
let test_security_requirements () =
  let secured schemes = Ocapi.Endpoint.Provide.make ~security:schemes (fun _ -> Ok ()) in
  let security provide =
    endpoint "/s"
      (let+ () = provide in
       "")
      [ echo ]
    |> Ocapi.Endpoint.Openapi.operation
    |> Yojson.Safe.Util.member "security"
    |> Yojson.Safe.to_string
  in
  Alcotest.(check string)
    "alternatives, and both" {|[{"a":[],"c":[]},{"b":[],"c":[]}]|}
    (security
       (let+ () = secured [ "a"; "b" ] and+ () = secured [ "c" ] in
        ()));
  Alcotest.(check string)
    "one scheme twice is one" {|[{"a":[]}]|}
    (security
       (let+ () = secured [ "a" ] and+ () = secured [ "a" ] in
        ()))

let test_openapi_document () =
  let a =
    endpoint ~meth:`POST "/a"
      (Ocapi.Endpoint.Body.raw ~name:"request" ~schema:string_schema ~content_type:"text/plain"
         Fun.id)
      [ echo ]
  in
  Alcotest.(check (list string))
    "top-level keys, in order"
    [ "openapi"; "info"; "servers"; "paths"; "components" ]
    (match
       Ocapi.Endpoint.Openapi.document
         ~servers:[ `Assoc [ "url", `String "http://localhost" ] ]
         ~info:(`Assoc [ "title", `String "t"; "version", `String "1" ])
         [ a ]
     with
    | `Assoc fields -> List.map fst fields
    | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> []);
  Alcotest.(check string)
    "components"
    {|{"securitySchemes":{"bearerAuth":{"type":"http","scheme":"bearer"}},"schemas":{"Request":{"type":"string"},"Echo":{"type":"string"}}}|}
    (Ocapi.Endpoint.Openapi.document
       ~security_schemes:
         [ "bearerAuth", `Assoc [ "type", `String "http"; "scheme", `String "bearer" ] ]
       ~info:(`Assoc []) [ a ]
    |> Yojson.Safe.Util.member "components"
    |> Yojson.Safe.to_string)

let () =
  Alcotest.run "endpoint"
    [
      ( "make",
        [
          Alcotest.test_case "template must match path parameters" `Quick
            test_template_must_match_the_path_parameters;
          Alcotest.test_case "at least one response" `Quick test_at_least_one_response;
        ] );
      ( "provide",
        [
          Alcotest.test_case "left to right" `Quick test_providers_resolve_left_to_right;
          Alcotest.test_case "query reads like Uri" `Quick test_query_reads_like_uri;
          Alcotest.test_case "optional and int" `Quick test_optional_and_int;
          Alcotest.test_case "require" `Quick test_require_turns_a_failure_into_a_reply;
          Alcotest.test_case "body" `Quick test_body;
          Alcotest.test_case "a failure to read the body propagates" `Quick
            test_a_failure_to_read_the_body_propagates;
        ] );
      ( "openapi",
        [
          Alcotest.test_case "operation" `Quick test_openapi_operation;
          Alcotest.test_case "paths" `Quick test_openapi_paths;
          Alcotest.test_case "endpoints of different types" `Quick test_endpoints_of_different_types;
          Alcotest.test_case "schemas" `Quick test_openapi_schemas;
          Alcotest.test_case "document" `Quick test_openapi_document;
          Alcotest.test_case "a reply decodes back" `Quick test_a_reply_decodes_back;
          Alcotest.test_case "a request encodes back" `Quick test_a_request_encodes_back;
          Alcotest.test_case "an empty case does not take a body" `Quick
            test_an_empty_case_does_not_take_a_body;
          Alcotest.test_case "templates are checked" `Quick test_templates_are_checked;
          Alcotest.test_case "the document refuses duplicates" `Quick
            test_the_document_refuses_duplicates;
          Alcotest.test_case "a raw body has its content type" `Quick
            test_a_raw_body_has_its_content_type;
          Alcotest.test_case "bool" `Quick test_bool;
          Alcotest.test_case "contramap" `Quick test_contramap;
          Alcotest.test_case "json with headers" `Quick test_json_with_headers;
          Alcotest.test_case "cookie parameters" `Quick test_cookie_parameters;
          Alcotest.test_case "a json rejection is documented" `Quick
            test_a_json_rejection_is_documented;
          Alcotest.test_case "header parameters" `Quick test_header_parameters;
          Alcotest.test_case "security requirements" `Quick test_security_requirements;
          Alcotest.test_case "responses from providers and transport" `Quick
            test_responses_from_providers_and_transport;
        ] );
    ]
