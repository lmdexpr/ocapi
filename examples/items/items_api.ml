open Ocapi

type item = { id : string; name : string }

let string_schema = `Assoc [ "type", `String "string" ]

let object_schema properties =
  `Assoc
    [
      "type", `String "object";
      "properties", `Assoc (List.map (fun name -> name, string_schema) properties);
      "required", `List (List.map (fun name -> `String name) properties);
    ]

let item_schema = object_schema [ "id"; "name" ]
let item_to_json { id; name } = `Assoc [ "id", `String id; "name", `String name ]

let item_of_json json =
  Yojson.Safe.Util.{ id = member "id" json |> to_string; name = member "name" json |> to_string }

let item = Endpoint.Codec.make ~name:"item" ~schema:item_schema item_to_json item_of_json

let items =
  Endpoint.Codec.make ~name:"items"
    ~schema:(`Assoc [ "type", `String "array"; "items", item_schema ])
    (fun items -> `List (List.map item_to_json items))
    (fun json -> Yojson.Safe.Util.to_list json |> List.map item_of_json)

let error =
  Endpoint.Codec.make ~name:"error" ~schema:(object_schema [ "error" ])
    (fun e -> `Assoc [ "error", `String e ])
    Yojson.Safe.Util.(fun json -> member "error" json |> to_string)

module Index = struct
  type request = { limit : int option }

  let endpoint =
    let open Endpoint.Syntax in
    Endpoint.make ~meth:`GET "/items" ~tags:[ "Item" ] ~summary:"Items"
      ~responses:Endpoint.[ Response.json ~status:200 ~description:"The items" items Prism.id ]
    @@
    let+ limit =
      Endpoint.Query.optional ~description:"At most this many items"
        (Endpoint.Value.int ~minimum:1 ~maximum:100 ()) "limit" (fun r -> r.limit)
      |> Endpoint.Provide.require_json ~status:400 ~description:"limit is not between 1 and 100"
           error (fun _ -> "invalid_limit")
    in
    { limit }
end

module Get = struct
  type request = { id : string }

  let endpoint =
    let open Endpoint.Syntax in
    Endpoint.make ~meth:`GET "/items/{id}" ~tags:[ "Item" ] ~summary:"One item"
      ~responses:
        Endpoint.
          [
            Response.json ~status:200 ~description:"The item" item Prism.ok;
            Response.json ~status:404 ~description:"No such item" error Prism.error;
          ]
    @@
    let+ id = Endpoint.Path.string ~description:"Item id" "id" (fun r -> r.id) in
    { id }
end

module Create = struct
  type request = { name : string }

  let new_item =
    Endpoint.Codec.make ~name:"new_item" ~schema:(object_schema [ "name" ])
      (fun name -> `Assoc [ "name", `String name ])
      Yojson.Safe.Util.(fun json -> member "name" json |> to_string)

  let endpoint =
    let open Endpoint.Syntax in
    Endpoint.make ~meth:`POST "/items" ~tags:[ "Item" ] ~summary:"Create an item"
      ~responses:
        Endpoint.[ Response.json ~status:201 ~description:"The created item" item Prism.id ]
    @@
    let+ name =
      Endpoint.Body.json new_item (fun r -> r.name)
      |> Endpoint.Provide.require_json ~status:400 ~description:"The body is not {\"name\": string}"
           error (fun (`Malformed _) -> "malformed_body")
    in
    { name }
end
