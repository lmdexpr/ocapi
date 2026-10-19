type schema = Ppx_deriving_jsonschema_runtime.t

(* RFC 6901 *)
let escape key =
  key |> String.split_on_char '~' |> String.concat "~0" |> String.split_on_char '/'
  |> String.concat "~1"

let component name = "#/components/schemas/" ^ escape (Ocapi.Endpoint.Openapi.component_name name)
let local = "#/$defs/"
let after prefix s = String.sub s (String.length prefix) (String.length s - String.length prefix)

let rec map_refs f = function
  | `Assoc fields ->
    `Assoc
      (fields
      |> List.map @@ fun (key, value) ->
         match key, value with
         | "$ref", `String target -> key, `String (f target)
         | key, value -> key, map_refs f value)
  | `List items -> `List (List.map (map_refs f) items)
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as leaf -> leaf

type scope = { pointer : string; defs : string list }

(* The ppx resolves [#/$defs/x] in the nearest object holding [$defs] (the schema itself, or an
   embedded schema it marked with a [file://] [$id]); a name defined there is a pointer into it.
   Any other name is a shared definition, a component of its own. *)
let rec rewrite ~scope ~pointer = function
  | `Assoc fields ->
    let scope =
      match List.assoc_opt "$defs" fields with
      | Some (`Assoc defs) -> { pointer; defs = List.map fst defs }
      | Some (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _) | None -> scope
    in
    `Assoc
      (fields
      |> List.filter_map @@ fun (key, value) ->
         match key, value with
         | "$id", `String id when String.starts_with ~prefix:"file://" id -> None
         | "$ref", `String target when String.starts_with ~prefix:local target ->
           let rest = after local target in
           let name = List.hd (String.split_on_char '/' rest) in
           if List.mem name scope.defs then
             Some (key, `String (scope.pointer ^ "/$defs/" ^ rest))
           else
             Some (key, `String (component name ^ after name rest))
         | key, value -> Some (key, rewrite ~scope ~pointer:(pointer ^ "/" ^ escape key) value))
  | `List items ->
    `List
      (items
      |> List.mapi @@ fun i item -> rewrite ~scope ~pointer:(pointer ^ "/" ^ string_of_int i) item)
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as leaf -> leaf

let schema ~name (s : schema) =
  let pointer = component name in
  rewrite ~scope:{ pointer; defs = [] } ~pointer (s :> Yojson.Safe.t)

let codec ~name s to_json of_json =
  Ocapi.Endpoint.Codec.make ~name ~schema:(schema ~name s) to_json of_json

let components definitions = definitions |> List.map @@ fun (name, s) -> name, schema ~name s

(* The inverse of [schema]: pointers into the component become pointers from the document root,
   references to a shared definition go back to [#/$defs]. *)
let standalone ~definitions name schema =
  let own = component name in
  let shared = definitions |> List.map (fun (d, _) -> component d, d) in
  let back target =
    match List.assoc_opt target shared with
    | Some d -> local ^ escape d
    | None when target = own -> "#"
    | None when String.starts_with ~prefix:(own ^ "/") target -> "#" ^ after own target
    | None -> target
  in
  match map_refs back schema with
  | `Assoc fields ->
    let own_defs =
      match List.assoc_opt "$defs" fields with
      | Some (`Assoc defs) -> defs
      | Some (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _) | None -> []
    in
    let defs =
      own_defs
      @ (definitions
        |> List.filter (fun (d, _) -> not (List.mem_assoc d own_defs))
        |> List.map (fun (d, (s : schema)) -> d, (s :> Yojson.Safe.t)))
    in
    `Assoc
      ([
         "$schema", `String Ppx_deriving_jsonschema_runtime.schema_version;
         "title", `String (Ocapi.Endpoint.Openapi.component_name name);
       ]
      @ (match defs with [] -> [] | defs -> [ "$defs", `Assoc defs ])
      @ List.remove_assoc "$defs" fields)
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
    invalid_arg ("schema " ^ name ^ " is not a JSON object")

let documents ?(definitions = []) endpoints =
  Ocapi.Endpoint.Openapi.schemas endpoints
  |> List.map @@ fun (name, schema) -> name, standalone ~definitions name schema
