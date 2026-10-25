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

let echo =
  Ocapi.Endpoint.Response.json ~status:200 ~description:"echo"
    (Ocapi.Endpoint.Codec.make ~name:"echo" ~schema:string_schema
       (fun s -> `String s)
       Yojson.Safe.Util.to_string)
    Ocapi.Endpoint.Prism.id

let endpoint ?(meth = `GET) ?headers ?hidden template provide responses =
  Ocapi.Endpoint.make ~meth template ?headers ?hidden ~tags:[] ~summary:"s" provide ~responses

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
            outside ~status:500 ~description:"internal" ();
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
    (Yojson.Safe.to_string (Ocapi_openapi.operation documented))

let test_openapi_paths () =
  let get = endpoint "/a" (Ocapi.Endpoint.Provide.return "") [ echo ] in
  let post = endpoint ~meth:`POST "/a" (Ocapi.Endpoint.Provide.return "") [ echo ] in
  let hidden = endpoint ~hidden:true "/h" (Ocapi.Endpoint.Provide.return "") [ echo ] in
  Alcotest.(check (list (pair string (list string))))
    "grouped by template, hidden left out"
    [ "/a", [ "get"; "post" ] ]
    (Ocapi_openapi.(paths [ get; post; hidden ])
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
  let first = Ocapi_openapi.Endpoints.[ text ] in
  Alcotest.(check (list string))
    "templates in order" [ "/text"; "/count" ]
    (Ocapi_openapi.(paths (Endpoints.append first [ count ])) |> List.map fst)

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
    (Ocapi_openapi.(schemas [ a; b; hidden ]) |> List.map fst);
  let conflicting = endpoint ~meth:`POST "/c" (body "request" string_schema) [ echo ] in
  Alcotest.check_raises "conflict"
    (Invalid_argument "schema request is declared with two different schemas") (fun () ->
    ignore Ocapi_openapi.(schemas [ a; conflicting ]))

(* One status, described by the most specific source: the cases, the providers, the transport.
   The endpoint headers go with what the endpoint answers, not with what the transport does. *)
let test_responses_from_providers_and_transport () =
  let common : Ocapi.Endpoint.common list =
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
    match Ocapi_openapi.operation ~common endpoint |> Yojson.Safe.Util.member "responses" with
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
              outside ~status:200 ~description:"outside" ();
              outside ~status:500 ~description:"outside" ();
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

(* The document refuses what OpenAPI forbids rather than emitting it. *)
let test_the_document_refuses_duplicates () =
  let at template = endpoint template (Ocapi.Endpoint.Path.string "x" Fun.id) [ echo ] in
  let a = endpoint "/a" (Ocapi.Endpoint.Provide.return "") [ echo ] in
  let post = endpoint ~meth:`POST "/a" (Ocapi.Endpoint.Provide.return "") [ echo ] in
  Alcotest.check_raises "one method twice" (Invalid_argument "GET /a is declared twice") (fun () ->
    ignore Ocapi_openapi.(paths [ a; post; a ]));
  Alcotest.check_raises "paths differing in parameter names only"
    (Invalid_argument "paths /b/{x} and /b/{y} are the same path") (fun () ->
    ignore
      Ocapi_openapi.(
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
    (fun () -> ignore Ocapi_openapi.(schemas [ body "foo_bar"; body "foo__bar" ]))

(* Schemes of one provider are alternatives; every provider must be satisfied. *)
let test_security_requirements () =
  let secured schemes = Ocapi.Endpoint.Provide.make ~security:schemes (fun _ -> Ok ()) in
  let security provide =
    endpoint "/s"
      (let+ () = provide in
       "")
      [ echo ]
    |> Ocapi_openapi.operation
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
       Ocapi_openapi.document
         ~servers:[ `Assoc [ "url", `String "http://localhost" ] ]
         ~info:(`Assoc [ "title", `String "t"; "version", `String "1" ])
         [ a ]
     with
    | `Assoc fields -> List.map fst fields
    | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> []);
  Alcotest.(check string)
    "components"
    {|{"securitySchemes":{"bearerAuth":{"type":"http","scheme":"bearer"}},"schemas":{"Request":{"type":"string"},"Echo":{"type":"string"}}}|}
    (Ocapi_openapi.document
       ~security_schemes:
         [ "bearerAuth", `Assoc [ "type", `String "http"; "scheme", `String "bearer" ] ]
       ~info:(`Assoc []) [ a ]
    |> Yojson.Safe.Util.member "components"
    |> Yojson.Safe.to_string)

(* What the caller leaves out is left out of the document, except the description of a response,
   which OpenAPI requires: the reason phrase stands in. An unnamed schema is written inline. *)
let test_optional_documentation () =
  let open Ocapi.Endpoint in
  let inline = Codec.make ~schema:string_schema (fun s -> `String s) Yojson.Safe.Util.to_string in
  let e =
    make ~meth:`GET "/bare/{id}"
      (let+ id = Path.string "id" Fun.id in
       id)
      ~responses:
        Response.
          [
            json ~status:200 inline Prism.ok; empty ~status:404 Prism.error; outside ~status:599 ();
          ]
  in
  Alcotest.(check string)
    "operation"
    ({|{"parameters":[{"name":"id","in":"path","required":true,"schema":{"type":"string"}}],|}
   ^ {|"responses":{"200":{"description":"OK","content":{"application/json":{"schema":{"type":"string"}}}},|}
   ^ {|"404":{"description":"Not Found"},"599":{"description":"Status 599"}}}|})
    (Yojson.Safe.to_string (Ocapi_openapi.operation e));
  Alcotest.(check (list string)) "no components" [] (Ocapi_openapi.schemas [ e ] |> List.map fst)

let parameters_in operation =
  match Yojson.Safe.Util.member "parameters" operation with
  | `List ps -> ps |> List.map (fun p -> Yojson.Safe.Util.(member "in" p |> to_string))
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ -> []

(* A raw body is documented under its own media type, not JSON. *)
let test_a_raw_body_has_its_content_type () =
  let csv =
    endpoint ~meth:`POST "/csv"
      (Ocapi.Endpoint.Body.raw ~name:"table" ~schema:string_schema ~content_type:"text/csv" Fun.id)
      [ echo ]
  in
  Alcotest.(check (list string))
    "documented" [ "text/csv" ]
    (match
       Ocapi_openapi.operation csv
       |> Yojson.Safe.Util.member "requestBody"
       |> Yojson.Safe.Util.member "content"
     with
    | `Assoc media -> List.map fst media
    | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> [])

let test_header_and_cookie_parameters () =
  let open Ocapi.Endpoint in
  let e =
    endpoint "/t"
      (let+ _ = Header.required Value.string "X-Trace" Fun.id
       and+ _ = Header.optional Value.string "X-Retries" (fun _ -> None)
       and+ _ = Cookie.required Value.string "session" Fun.id in
       "")
      [ echo ]
  in
  Alcotest.(check (list string))
    "documented" [ "header"; "header"; "cookie" ]
    (parameters_in (Ocapi_openapi.operation e))

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
      (Ocapi.Endpoint.Query.required Ocapi.Endpoint.Value.string "k" Fun.id
      |> Ocapi.Endpoint.Provide.require_json ~status:422 ~description:"k is missing" error (fun _ ->
        "missing_k"))
      [ echo ]
  in
  Alcotest.(check string)
    "documented"
    {|{"description":"k is missing","content":{"application/json":{"schema":{"$ref":"#/components/schemas/Error"}}}}|}
    (Ocapi_openapi.operation e
    |> Yojson.Safe.Util.member "responses"
    |> Yojson.Safe.Util.member "422" |> Yojson.Safe.to_string);
  Alcotest.(check (list string))
    "in the components" [ "error"; "echo" ]
    (Ocapi_openapi.schemas [ e ] |> List.map fst)

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
  Alcotest.(check string)
    "documented" {|{"Set-Cookie":{"description":"the session","schema":{"type":"string"}}}|}
    (Ocapi_openapi.operation e
    |> Yojson.Safe.Util.(fun o -> member "responses" o |> member "200" |> member "headers")
    |> Yojson.Safe.to_string)

(* A case or provider left undescribed does not hide the description another one gives. *)
let test_the_first_description_written () =
  let open Ocapi.Endpoint in
  let named name =
    Codec.make ~name ~schema:string_schema (fun s -> `String s) Yojson.Safe.Util.to_string
  in
  let describe e status =
    Ocapi_openapi.operation
      ~common:[ { status = 500; description = "transport 500"; scope = `Every_operation } ]
      e
    |> Yojson.Safe.Util.(fun o -> member "responses" o |> member (string_of_int status))
    |> Yojson.Safe.Util.(fun r -> member "description" r |> to_string)
  in
  let cases =
    endpoint "/c" (Provide.return "")
      Response.
        [
          json ~status:200 (named "a") Prism.id;
          json ~status:200 ~description:"the item" (named "b") Prism.id;
          outside ~status:500 ();
        ]
  in
  Alcotest.(check (list string))
    "from a later case, from the transport" [ "the item"; "transport 500" ]
    [ describe cases 200; describe cases 500 ];
  let providers =
    endpoint "/p"
      (let+ _ =
         Query.required Value.string "k" Fun.id
         |> Provide.require_json ~status:400 (named "error") (fun _ -> "")
       and+ _ = Provide.make ~responses:[ 400, "k is invalid" ] (fun _ -> Ok ()) in
       "")
      [ echo ]
  in
  Alcotest.(check string) "from a later provider" "k is invalid" (describe providers 400)

let test_reason_phrases () =
  Alcotest.(check (list string))
    "known and unknown"
    [ "OK"; "Content Too Large"; "Status 299" ]
    (List.map Ocapi_openapi.reason_phrase [ 200; 413; 299 ])

let () =
  Alcotest.run "openapi"
    [
      ( "document",
        [
          Alcotest.test_case "operation" `Quick test_openapi_operation;
          Alcotest.test_case "paths" `Quick test_openapi_paths;
          Alcotest.test_case "endpoints of different types" `Quick test_endpoints_of_different_types;
          Alcotest.test_case "schemas" `Quick test_openapi_schemas;
          Alcotest.test_case "document" `Quick test_openapi_document;
          Alcotest.test_case "the document refuses duplicates" `Quick
            test_the_document_refuses_duplicates;
          Alcotest.test_case "a raw body has its content type" `Quick
            test_a_raw_body_has_its_content_type;
          Alcotest.test_case "header and cookie parameters" `Quick test_header_and_cookie_parameters;
          Alcotest.test_case "a json rejection is documented" `Quick
            test_a_json_rejection_is_documented;
          Alcotest.test_case "json with headers" `Quick test_json_with_headers;
          Alcotest.test_case "security requirements" `Quick test_security_requirements;
          Alcotest.test_case "responses from providers and transport" `Quick
            test_responses_from_providers_and_transport;
          Alcotest.test_case "optional documentation" `Quick test_optional_documentation;
          Alcotest.test_case "reason phrases" `Quick test_reason_phrases;
          Alcotest.test_case "the first description written" `Quick
            test_the_first_description_written;
        ] );
    ]
