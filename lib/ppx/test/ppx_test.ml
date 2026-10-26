open Ppx_deriving_jsonschema_runtime.Primitives.Yojson
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type item = { id : string; name : string } [@@deriving yojson, jsonschema, ocapi]
type error = { error : string } [@@deriving yojson, jsonschema, ocapi]
type tag = Tag of string

let tag_value =
  Ocapi.Endpoint.Value.make ~schema:(`Assoc []) (fun s -> Some (Tag s)) (fun (Tag s) -> s)

module Get = struct
  module Spec = struct
    include Ocapi.Endpoint.Defaults

    type request = {
      id : string;  (** Item id *)
      verbose : bool; [@default false]
      tag : tag option; [@query "t"]
      trace : string; [@header "X-Trace"]
    }
    [@@deriving provide ~path:"/items/{id}"]
    (** One item

        Returns the item with the given id. *)

    type result =
      | Found of item [@status 200]  (** The item *)
      | Not_found of error [@status 404]
      | Gone [@status 410]
    [@@deriving responses]

    let meth = `GET
    let tags = [ "Item" ]
  end

  include Spec
  include Ocapi.Endpoint.Make (Spec)
end

module Create = struct
  module Spec = struct
    include Ocapi.Endpoint.Defaults

    type request = { item : item [@body] } [@@deriving provide ~path:"/items"]
    type result = Created of item [@status 201] [@@deriving responses]

    let meth = `POST
  end

  include Spec
  include Ocapi.Endpoint.Make (Spec)
end

(* A provider written apart, for [@provide]. *)
let page =
  Ocapi.Endpoint.Query.required (Ocapi.Endpoint.Value.int ()) "page" Fun.id
  |> Ocapi.Endpoint.Provide.require (fun _ -> Ocapi.Endpoint.Reply.empty 400)

module Search = struct
  module Spec = struct
    include Ocapi.Endpoint.Defaults

    type request = {
      q : string;  (** The words, e.g. [ocaml effects] *)
      limit : int option; [@value Ocapi.Endpoint.Value.int ~maximum:10 ()]
      session : string; [@cookie]
      page : int; [@provide page]
    }
    [@@deriving provide ~path:"/search"]
    (** Search {b items} by [q]

        Matches {i whole} words; see {{:https://example.com/search}the guide}.

        - one word
        - [AND] between words

        @since 1.0 *)

    type result =
      | Hit of item [@status 200] [@discriminator "kind", "hit"]
      | Miss of error [@status 200] [@codec error_codec] [@discriminator "kind", "miss"]
    [@@deriving responses]

    let meth = `GET
  end

  include Spec
  include Ocapi.Endpoint.Make (Spec)
end

let routes =
  Ocapi.Server.
    [
      route
        (module Search)
        (fun { q; limit = _; session; page } ->
          if session = "s1" then
            Hit { id = string_of_int page; name = q }
          else
            Miss { error = "no session" });
      route
        (module Get)
        (fun { id; verbose; tag; trace = _ } ->
          match id, tag with
          | "gone", _ -> Gone
          | "a1", (None | Some (Tag "x")) ->
            Found { id; name = (if verbose then "the item a1" else "a1") }
          | _ -> Not_found { error = "not_found" });
      route (module Create) (fun { item } -> Created item);
    ]

let in_process f =
  try f ()
  with effect Ocapi.Client.Send (r : Ocapi.Client.request), k ->
    let reply =
      Ocapi.Server.serve routes
        {
          meth = r.meth;
          segments = String.split_on_char '/' r.path |> List.filter (( <> ) "");
          query = r.query;
          header = (fun name -> Ocapi.Client.header name r.headers);
          body = (fun () -> Option.value r.body ~default:"");
          params = [];
        }
    in
    let body =
      match reply.body with Json j -> Yojson.Safe.to_string j | Empty -> "" | Raw s -> s
    in
    Effect.Deep.continue k Ocapi.Client.{ status = reply.status; headers = reply.headers; body }

(* The derived declaration travels both ways, through every kind of field and case. *)
let test_round_trip () =
  in_process @@ fun () ->
  let get ?(verbose = false) ?tag id =
    Ocapi.Client.invoke (module Get) { id; verbose; tag; trace = "t1" }
  in
  Alcotest.(check bool)
    "found" true
    (get ~verbose:true "a1" = Ok (Get.Found { id = "a1"; name = "the item a1" }));
  Alcotest.(check bool) "default" true (get "a1" = Ok (Get.Found { id = "a1"; name = "a1" }));
  Alcotest.(check bool)
    "custom value" true
    (get ~tag:(Tag "y") "a1" = Ok (Get.Not_found { error = "not_found" }));
  Alcotest.(check bool) "no body" true (get "gone" = Ok Get.Gone);
  let search ?limit session =
    Ocapi.Client.invoke (module Search) { q = "ocaml"; limit; session; page = 2 }
  in
  Alcotest.(check bool)
    "a cookie and a provider" true
    (search "s1" = Ok (Search.Hit { id = "2"; name = "ocaml" }));
  Alcotest.(check bool)
    "another case of the status" true
    (search "s2" = Ok (Search.Miss { error = "no session" }));
  Alcotest.(check bool)
    "a value of the field's own" true
    (match search ~limit:11 "s1" with Error (`Invalid _) -> true | Ok _ | Error _ -> false);
  Alcotest.(check bool)
    "body" true
    (Ocapi.Client.invoke (module Create) { item = { id = "b"; name = "bee" } }
    = Ok (Create.Created { id = "b"; name = "bee" }));
  let missing =
    Ocapi.Server.serve routes
      {
        meth = `GET;
        segments = [ "items"; "a1" ];
        query = [];
        header = (fun _ -> None);
        body = (fun () -> "");
        params = [];
      }
  in
  Alcotest.(check int) "a missing header" 400 missing.status

(* Doc comments, field locations, the module path and the cases make the document. *)
(* odoc markup becomes CommonMark; the summary keeps the text only, tags are left out. *)
let test_odoc_markup () =
  let operation = Ocapi_openapi.operation Search.endpoint in
  let member key json = Yojson.Safe.Util.member key json in
  Alcotest.(check (list string))
    "summary, description and a parameter"
    [
      "Search items by q";
      "Matches *whole* words; see [the guide](https://example.com/search).\n\n\
       - one word\n\
       - `AND` between words";
      "The words, e.g. `ocaml effects`";
    ]
    [
      member "summary" operation |> Yojson.Safe.Util.to_string;
      member "description" operation |> Yojson.Safe.Util.to_string;
      Yojson.Safe.Util.(
        member "parameters" operation |> index 0 |> member "description" |> to_string);
    ]

(* Where each field travels, the field's own value, and the discriminator of one status. *)
let test_document_of_search () =
  let operation = Ocapi_openapi.operation Search.endpoint in
  let member key json = Yojson.Safe.Util.member key json in
  Alcotest.(check string)
    "parameters"
    ({|[{"name":"q","in":"query","required":true,"description":"The words, e.g. `ocaml effects`",|}
   ^ {|"schema":{"type":"string"}},|}
   ^ {|{"name":"limit","in":"query","required":false,"schema":{"type":"integer","maximum":10}},|}
   ^ {|{"name":"session","in":"cookie","required":true,"schema":{"type":"string"}},|}
   ^ {|{"name":"page","in":"query","required":true,"schema":{"type":"integer"}}]|})
    (member "parameters" operation |> Yojson.Safe.to_string);
  Alcotest.(check string)
    "discriminator"
    {|{"propertyName":"kind","mapping":{"hit":"#/components/schemas/Item","miss":"#/components/schemas/Error"}}|}
    (operation |> member "responses" |> member "200" |> member "content"
   |> member "application/json" |> member "schema" |> member "discriminator"
   |> Yojson.Safe.to_string)

let test_document () =
  let operation = Ocapi_openapi.operation Get.endpoint in
  let member key json = Yojson.Safe.Util.member key json in
  Alcotest.(check (list string))
    "operation"
    [ {|"One item"|}; {|"Returns the item with the given id."|}; {|"get"|}; {|["Item"]|} ]
    ([ "summary"; "description"; "operationId"; "tags" ]
    |> List.map (fun key -> member key operation |> Yojson.Safe.to_string));
  Alcotest.(check string)
    "parameters"
    ({|[{"name":"id","in":"path","required":true,"description":"Item id","schema":{"type":"string"}},|}
   ^ {|{"name":"verbose","in":"query","required":false,"schema":{"type":"boolean"}},|}
   ^ {|{"name":"t","in":"query","required":false,"schema":{}},|}
   ^ {|{"name":"X-Trace","in":"header","required":true,"schema":{"type":"string"}}]|})
    (member "parameters" operation |> Yojson.Safe.to_string);
  Alcotest.(check (list string))
    "responses"
    [ "200 The item"; "400 verbose is invalid"; "404 Not Found"; "410 Gone" ]
    (match member "responses" operation with
    | `Assoc responses ->
      responses
      |> List.map (fun (status, r) ->
        status ^ " " ^ Yojson.Safe.Util.(member "description" r |> to_string))
    | _ -> []);
  Alcotest.(check string)
    "constructors" {|[{"constructor":"Found","schema":{"$ref":"#/components/schemas/Item"}}]|}
    (operation |> member "responses" |> member "200" |> member "x-ocapi-cases"
   |> Yojson.Safe.to_string);
  Alcotest.(check string)
    "a body from its codec" {|{"$ref":"#/components/schemas/Item"}|}
    (Ocapi_openapi.operation Create.endpoint
    |> member "requestBody" |> member "content" |> member "application/json" |> member "schema"
    |> Yojson.Safe.to_string)

let () =
  Alcotest.run "ppx"
    [
      ( "derived",
        [
          Alcotest.test_case "round trip" `Quick test_round_trip;
          Alcotest.test_case "document" `Quick test_document;
          Alcotest.test_case "document of search" `Quick test_document_of_search;
          Alcotest.test_case "odoc markup" `Quick test_odoc_markup;
        ] );
    ]
