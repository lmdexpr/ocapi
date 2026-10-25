let test_bearer_token () =
  let check name expected value =
    Alcotest.(check (option string)) name expected (Bearer_auth.bearer_token value)
  in
  check "bearer" (Some "abc") "Bearer abc";
  check "scheme is case-insensitive" (Some "abc") "bearer abc";
  check "trailing space is trimmed" (Some "abc") "Bearer abc  ";
  check "other scheme" None "Basic abc";
  check "no token" None "Bearer ";
  check "no scheme" None "abc"

let request header : Ocapi.Endpoint.Request.t =
  {
    meth = `GET;
    segments = [ "me" ];
    query = [];
    header = (fun name -> if name = "authorization" then header else None);
    body = (fun () -> "");
    params = [];
  }

let test_credential_both_ways () =
  let provide = Bearer_auth.credential ~scheme:"bearer" Fun.id in
  Alcotest.(check (result (option string) reject))
    "present" (Ok (Some "t"))
    (Ocapi.Endpoint.Provide.resolve provide (request (Some "Bearer t")));
  Alcotest.(check (result (option string) reject))
    "absent" (Ok None)
    (Ocapi.Endpoint.Provide.resolve provide (request None));
  Alcotest.(check (list (pair string string)))
    "the client sends it"
    [ "authorization", "Bearer t" ]
    (Ocapi.Endpoint.Provide.encode provide (Some "t") |> Result.get_ok).headers;
  Alcotest.(check (list (pair string string)))
    "or nothing" [] (Ocapi.Endpoint.Provide.encode provide None |> Result.get_ok).headers

module User = Bearer_auth.Make (struct
  type t = string

  let scheme = "session"
end)

let test_the_handler_reads_the_user () =
  let me =
    Ocapi.Endpoint.make ~meth:`GET "/me" ~tags:[] ~summary:"s" (User.required ())
      ~responses:
        [
          Ocapi.Endpoint.Response.unspecified_json ~status:200 ~description:""
            Ocapi.Endpoint.Prism.id;
        ]
  in
  Alcotest.(check (option string))
    "the handler's user" (Some {|"alice"|})
    ( User.run "alice" @@ fun () ->
      Ocapi.Server.dispatch
        [ me |> Ocapi.Server.handle (fun () -> `String (User.current ())) ]
        (request None)
      |> Option.map (fun (reply : Ocapi.Endpoint.Reply.t) ->
        match reply.body with Json json -> Yojson.Safe.to_string json | Empty | Raw _ -> "") )

(* The documented security is the scheme, nothing else is read from the request. *)
let test_required_documents_the_scheme () =
  let endpoint =
    Ocapi.Endpoint.make ~meth:`GET "/me" ~tags:[] ~summary:"s" (User.required ())
      ~responses:
        [ Ocapi.Endpoint.Response.empty ~status:204 ~description:"" Ocapi.Endpoint.Prism.id ]
  in
  match Ocapi_openapi.operation endpoint with
  | `Assoc fields ->
    Alcotest.(check string)
      "security" {|[{"session":[]}]|}
      (Yojson.Safe.to_string (List.assoc "security" fields));
    Alcotest.(check string)
      "401" {|{"description":"Not authenticated"}|}
      (Yojson.Safe.to_string (List.assoc "responses" fields |> Yojson.Safe.Util.member "401"))
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
    Alcotest.fail "expected an object"

let () =
  Alcotest.run "bearer_auth"
    [
      ( "bearer",
        [
          Alcotest.test_case "bearer_token" `Quick test_bearer_token;
          Alcotest.test_case "credential, both ways" `Quick test_credential_both_ways;
        ] );
      ( "required",
        [
          Alcotest.test_case "the handler reads the user" `Quick test_the_handler_reads_the_user;
          Alcotest.test_case "documents the scheme" `Quick test_required_documents_the_scheme;
        ] );
    ]
