open Ppx_deriving_jsonschema_runtime.Primitives.Yojson

type item = { id : string; count : int } [@@deriving jsonschema]
type tree = Leaf | Node of { left : tree; value : int; right : tree } [@@deriving jsonschema]
type forest = { name : string; trees : tree list } [@@deriving jsonschema]

let codec name schema = Ocapi_jsonschema.codec ~name schema (fun _ -> `Null) (fun _ -> ())

(* A shared definition, referred to the way a hand-written schema or [~definitions] does. *)
let period : Ocapi_jsonschema.schema =
  `Assoc
    [
      "type", `String "object";
      "properties", `Assoc [ "start", `Assoc [ "type", `String "string" ] ];
    ]

let booking : Ocapi_jsonschema.schema =
  `Assoc
    [
      "type", `String "object";
      "properties", `Assoc [ "when", `Assoc [ "$ref", `String "#/$defs/period" ] ];
    ]

let definitions = [ "period", period ]

let endpoint =
  Ocapi.Endpoint.make ~meth:`POST "/forests" ~tags:[] ~summary:"s"
    (Ocapi.Endpoint.Body.json
       (Ocapi_jsonschema.codec ~name:"item_request" item_jsonschema (fun _ -> `Null) (fun _ -> ()))
       Fun.id
    |> Ocapi.Endpoint.Provide.require (fun _ -> Ocapi.Endpoint.Reply.empty 400))
    ~responses:
      Ocapi.Endpoint.Response.
        [
          json ~status:200 ~description:"" (codec "tree" tree_jsonschema) Ocapi.Endpoint.Prism.id;
          json ~status:201 ~description:""
            (codec "forest" forest_jsonschema)
            Ocapi.Endpoint.Prism.id;
          json ~status:202 ~description:"" (codec "booking" booking) Ocapi.Endpoint.Prism.id;
        ]

let document =
  Ocapi_openapi.document ~info:(`Assoc [])
    ~schemas:(Ocapi_jsonschema.components definitions)
    [ endpoint ]

let rec refs = function
  | `Assoc fields -> (
    fields
    |> List.concat_map @@ fun (key, value) ->
       match key, value with "$ref", `String target -> [ target ] | _, value -> refs value)
  | `List items -> List.concat_map refs items
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ -> []

let unescape key =
  key
  |> Str.global_replace (Str.regexp_string "~1") "/"
  |> Str.global_replace (Str.regexp_string "~0") "~"

let resolve target =
  match String.split_on_char '/' target with
  | "#" :: keys ->
    keys
    |> List.fold_left
         (fun node key ->
           match node with
           | Some (`Assoc fields) -> List.assoc_opt (unescape key) fields
           | Some (`List items) ->
             int_of_string_opt key |> Fun.flip Option.bind (List.nth_opt items)
           | Some (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) | None -> None)
         (Some document)
  | [] | _ :: _ -> None

(* Every reference, recursive types included, resolves from the document root. *)
let test_every_reference_resolves () =
  let targets = refs document in
  Alcotest.(check bool)
    "the recursive types refer to their definitions" true
    (List.exists (String.starts_with ~prefix:"#/components/schemas/Tree/$defs/") targets
    && List.exists (String.starts_with ~prefix:"#/components/schemas/Forest/") targets);
  targets
  |> List.iter @@ fun target -> Alcotest.(check bool) target true (Option.is_some (resolve target))

(* The ppx marks embedded schemas with the source file; the document does not carry it. *)
let test_no_source_path_is_published () =
  let mentions_a_file json =
    Str.string_match (Str.regexp ".*file://") (Yojson.Safe.to_string json) 0
  in
  Alcotest.(check bool)
    "the derived schema has one" true
    (mentions_a_file (forest_jsonschema : Ocapi_jsonschema.schema :> Yojson.Safe.t));
  Alcotest.(check bool) "the document has none" false (mentions_a_file document)

let test_a_schema_without_definitions_is_kept () =
  Alcotest.(check string)
    "unchanged"
    (Yojson.Safe.to_string (item_jsonschema : Ocapi_jsonschema.schema :> Yojson.Safe.t))
    (Yojson.Safe.to_string (Ocapi_jsonschema.schema ~name:"item" item_jsonschema))

let documents = Ocapi_jsonschema.documents ~definitions [ endpoint ]

(* A standalone file has the shared definitions in its [$defs], referred to from there. *)
let test_a_document_carries_the_shared_definitions () =
  let booking = List.assoc "booking" documents in
  Alcotest.(check string)
    "title" {|"Booking"|}
    (Yojson.Safe.Util.member "title" booking |> Yojson.Safe.to_string);
  Alcotest.(check string)
    "the definition"
    (Yojson.Safe.to_string (period :> Yojson.Safe.t))
    (Yojson.Safe.Util.(member "$defs" booking |> member "period") |> Yojson.Safe.to_string);
  Alcotest.(check string)
    "the reference" {|{"$ref":"#/$defs/period"}|}
    (Yojson.Safe.Util.(member "properties" booking |> member "when") |> Yojson.Safe.to_string)

(* A recursive type's file is the ppx's own form again. *)
let test_a_document_of_a_recursive_type_is_the_ppx_form () =
  let tree = List.assoc "tree" documents in
  let original = (tree_jsonschema : Ocapi_jsonschema.schema :> Yojson.Safe.t) in
  Alcotest.(check string)
    "$ref"
    (Yojson.Safe.Util.member "$ref" original |> Yojson.Safe.to_string)
    (Yojson.Safe.Util.member "$ref" tree |> Yojson.Safe.to_string);
  Alcotest.(check string)
    "$defs, with the shared definitions added"
    (Yojson.Safe.to_string
       (match Yojson.Safe.Util.member "$defs" original with
       | `Assoc own -> `Assoc (own @ [ "period", (period :> Yojson.Safe.t) ])
       | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> `Null))
    (Yojson.Safe.Util.member "$defs" tree |> Yojson.Safe.to_string)

let () =
  Alcotest.run "jsonschema"
    [
      ( "schema",
        [
          Alcotest.test_case "every reference resolves" `Quick test_every_reference_resolves;
          Alcotest.test_case "no source path is published" `Quick test_no_source_path_is_published;
          Alcotest.test_case "a schema without definitions is kept" `Quick
            test_a_schema_without_definitions_is_kept;
          Alcotest.test_case "a document carries the shared definitions" `Quick
            test_a_document_carries_the_shared_definitions;
          Alcotest.test_case "a document of a recursive type is the ppx form" `Quick
            test_a_document_of_a_recursive_type_is_the_ppx_form;
        ] );
    ]
