open Ocapi
open Ppx_deriving_jsonschema_runtime.Primitives.Yojson
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type item = { id : string; name : string } [@@deriving yojson, jsonschema, ocapi]
type items = item list [@@deriving yojson, jsonschema, ocapi]
type error = { error : string } [@@deriving yojson, jsonschema, ocapi]
type new_item = { name : string } [@@deriving yojson, jsonschema, ocapi]

let reject status description error =
  Endpoint.Provide.require_json ~status ~description error_codec (fun _ -> { error })

module Index = struct
  module Spec = struct
    include Endpoint.Defaults

    type request = {
      limit : int option;
         [@value Endpoint.Value.int ~minimum:1 ~maximum:100 ()]
         [@reject reject 400 "limit is not between 1 and 100" "invalid_limit"]
        (** At most this many items *)
    }
    [@@deriving provide ~path:"/items"]
    (** Items *)

    (* A result that is not a variant keeps its responses written out. *)
    type result = item list

    let meth = `GET
    let tags = [ "Item" ]
    let operation_id = Some "listItems"

    let responses =
      Endpoint.[ Response.json ~status:200 ~description:"The items" items_codec Prism.id ]
  end

  include Spec
  include Endpoint.Make (Spec)
end

module Get = struct
  module Spec = struct
    include Endpoint.Defaults

    type request = { id : string  (** Item id *) } [@@deriving provide ~path:"/items/{id}"]
    (** One item *)

    type result =
      | Found of item [@status 200]  (** The item *)
      | Not_found of error [@status 404]  (** No such item *)
    [@@deriving responses]

    let meth = `GET
    let tags = [ "Item" ]
    let operation_id = Some "getItem"
  end

  include Spec
  include Endpoint.Make (Spec)
end

module Create = struct
  module Spec = struct
    include Endpoint.Defaults

    type request = {
      item : new_item;
         [@body] [@reject reject 400 "The body is not {\"name\": string}" "malformed_body"]
    }
    [@@deriving provide ~path:"/items"]
    (** Create an item *)

    type result = Created of item [@status 201]  (** The created item *) [@@deriving responses]

    let meth = `POST
    let tags = [ "Item" ]
    let operation_id = Some "createItem"
  end

  include Spec
  include Endpoint.Make (Spec)
end
