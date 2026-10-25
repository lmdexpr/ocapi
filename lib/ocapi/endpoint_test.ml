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

(* A raw body travels under its own media type, not JSON. *)
let test_a_raw_body_has_its_content_type () =
  let csv =
    endpoint ~meth:`POST "/csv"
      (Ocapi.Endpoint.Body.raw ~name:"table" ~schema:string_schema ~content_type:"text/csv" Fun.id)
      [ echo ]
  in
  Alcotest.(check (option string))
    "sent" (Some "text/csv")
    ( Ocapi.Endpoint.encode csv "a,b" |> Result.get_ok |> fun encoded ->
      Ocapi.Client.header "content-type" encoded.headers )

type traced = { trace : string; retries : int option }

(* A header both ways: written by the client, read back by the server. *)
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
  Alcotest.check_raises "Authorization is security"
    (Invalid_argument "the header Authorization is not a parameter; declare it as security or media")
    (fun () -> ignore (Header.required ~description:"" Value.string "Authorization" Fun.id))

(* A rejection is sent with its codec. *)
let test_a_json_rejection () =
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
    = Error (Ocapi.Endpoint.Reply.json 422 (`Assoc [ "error", `String "missing_k" ])))

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
    = Ok ([ "set-cookie", "s=1" ], "alice"))

type session = { session : string; theme : string option }

(* Cookies are read by name from the Cookie header. *)
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

(* Parts without a schema travel like the others; only the document refuses them, at compile
   time ([Ocapi_openapi.operation] takes a [complete] endpoint). *)
let test_schemaless_parts_travel () =
  let open Ocapi.Endpoint in
  let upper =
    Value.custom
      (fun s -> if String.uppercase_ascii s = s then Some s else None)
      String.lowercase_ascii
  in
  let e =
    make ~meth:`GET "/u/{id}"
      (let+ id = Path.value upper "id" Fun.id in
       id)
      ~responses:
        [
          Response.json ~status:200
            (Codec.custom (fun s -> `String s) Yojson.Safe.Util.to_string)
            Prism.id;
        ]
  in
  Alcotest.(check bool) "served" true (run e Fun.id "/u/AB" = Some (Reply.json 200 (`String "AB")));
  Alcotest.(check bool) "not served" true (run e Fun.id "/u/ab" = Some (Reply.empty 404));
  Alcotest.(check bool)
    "the client refuses what the server would not parse" true
    (Result.is_error (encode e "AB"))

(* Discriminators that the document could not map are refused when the endpoint is made, whether
   or not the cases end up in a oneOf. *)
let test_discriminators_are_checked () =
  let open Ocapi.Endpoint in
  let named name =
    Codec.make ~name ~schema:string_schema (fun s -> `String s) Yojson.Safe.Util.to_string
  in
  let a = Prism.make ~inject:(fun s -> `A s) ~project:(function `A s -> Some s | `B _ -> None) in
  let b = Prism.make ~inject:(fun s -> `B s) ~project:(function `B s -> Some s | `A _ -> None) in
  let make ?da ?db ?(codec_a = named "a") ?(codec_b = named "b") () =
    ignore
      (endpoint "/d" (Provide.return "")
         Response.
           [
             json ~status:200 ?discriminator:da codec_a a;
             json ~status:200 ?discriminator:db codec_b b;
           ])
  in
  let refused name reason f =
    Alcotest.check_raises name
      (Invalid_argument ("the discriminator of 200 in GET /d: " ^ reason))
      f
  in
  make ();
  make ~da:("kind", "a") ~db:("kind", "b") ();
  make ~da:("kind", "a") ~db:("kind", "b") ~codec_b:(named "a") ();
  refused "some" "some cases have none" (make ~da:("kind", "a"));
  refused "one schema, some" "some cases have none" (make ~da:("kind", "a") ~codec_b:(named "a"));
  refused "two properties" "the properties differ" (make ~da:("kind", "a") ~db:("type", "b"));
  refused "unnamed" "a case without a named codec"
    (make ~da:("kind", "a") ~db:("kind", "b")
       ~codec_b:(Codec.make ~schema:string_schema (fun s -> `String s) Yojson.Safe.Util.to_string));
  refused "one value, two schemas" "the value x stands for two schemas"
    (make ~da:("kind", "x") ~db:("kind", "x"))

module Greet = struct
  module Spec = struct
    type request = { name : string }
    type nonrec result = (string, string) result
    type body = Ocapi.Endpoint.Provide.body_unread
    type doc = Ocapi.Endpoint.complete

    include Ocapi.Endpoint.Defaults

    let meth = `GET
    let path = "/greet/{name}"
    let operation_id = Some "greet"

    let provide =
      let+ name = Ocapi.Endpoint.Path.string "name" (fun r -> r.name) in
      { name }

    let responses =
      Ocapi.Endpoint.
        [
          Response.json ~status:200 string_codec Prism.ok;
          Response.json ~status:404 string_codec Prism.error;
        ]
  end

  include Spec
  include Ocapi.Endpoint.Make (Spec)
end

(* A module made with [Make] is routed and called whole, the request record in its own module. *)
let test_one_endpoint_one_module () =
  let routes =
    [
      Ocapi.Server.route
        (module Greet)
        (fun { name } -> if name = "nobody" then Error "unknown" else Ok ("hello " ^ name));
    ]
  in
  let send (r : Ocapi.Client.request) : Ocapi.Client.response =
    let reply =
      Ocapi.Server.serve routes
        { (request ~meth:r.meth ~query:r.query r.path) with header = (fun _ -> None) }
    in
    let body =
      match reply.body with Json j -> Yojson.Safe.to_string j | Empty -> "" | Raw s -> s
    in
    { status = reply.status; headers = reply.headers; body }
  in
  let call name =
    try Ocapi.Client.invoke (module Greet) { name }
    with effect Ocapi.Client.Send r, k -> Effect.Deep.continue k (send r)
  in
  Alcotest.(check bool) "ok" true (call "alice" = Ok (Ok "hello alice"));
  Alcotest.(check bool) "error" true (call "nobody" = Ok (Error "unknown"));
  Alcotest.check_raises "checked when applied"
    (Invalid_argument "path parameters of /greet do not match its template") (fun () ->
    let module Bad = Ocapi.Endpoint.Make (struct
      include Greet.Spec

      let path = "/greet"
    end) in
    ignore Bad.endpoint)

let () =
  Alcotest.run "endpoint"
    [
      ( "make",
        [
          Alcotest.test_case "template must match path parameters" `Quick
            test_template_must_match_the_path_parameters;
          Alcotest.test_case "at least one response" `Quick test_at_least_one_response;
          Alcotest.test_case "templates are checked" `Quick test_templates_are_checked;
          Alcotest.test_case "one endpoint, one module" `Quick test_one_endpoint_one_module;
          Alcotest.test_case "discriminators are checked" `Quick test_discriminators_are_checked;
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
          Alcotest.test_case "bool" `Quick test_bool;
          Alcotest.test_case "contramap" `Quick test_contramap;
          Alcotest.test_case "a raw body has its content type" `Quick
            test_a_raw_body_has_its_content_type;
          Alcotest.test_case "header parameters" `Quick test_header_parameters;
          Alcotest.test_case "cookie parameters" `Quick test_cookie_parameters;
          Alcotest.test_case "a json rejection" `Quick test_a_json_rejection;
          Alcotest.test_case "schemaless parts travel" `Quick test_schemaless_parts_travel;
        ] );
      ( "round trips",
        [
          Alcotest.test_case "a reply decodes back" `Quick test_a_reply_decodes_back;
          Alcotest.test_case "a request encodes back" `Quick test_a_request_encodes_back;
          Alcotest.test_case "an empty case does not take a body" `Quick
            test_an_empty_case_does_not_take_a_body;
          Alcotest.test_case "json with headers" `Quick test_json_with_headers;
        ] );
    ]
