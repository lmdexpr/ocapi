(** The OpenAPI 3.1 document of {!Ocapi.Endpoint} values, read through {!Ocapi.Endpoint.Meta}.

    Only {!Ocapi.Endpoint.complete} endpoints are documented: an endpoint with a part without a
    schema does not compile into the document. *)

(** Endpoints of any request and result types, written as a list literal:
    [Ocapi_openapi.document ~info [ get; post ]] where an {!Endpoints.t} is expected, or
    [Ocapi_openapi.Endpoints.[ get; post ]] anywhere else. Without either, the literal is an
    ordinary list and endpoints of different types do not fit in it. *)
module Endpoints : sig
  type t = [] : t | ( :: ) : (_, _, Ocapi.Endpoint.complete) Ocapi.Endpoint.t * t -> t

  val append : t -> t -> t
end

val component_name : string -> string
(** The components name (PascalCase) of a schema name (snake_case). *)

val reason_phrase : int -> string
(** What documents a response without a description: [OK], [Not Found], ... *)

val operation :
  ?common:Ocapi.Endpoint.common list ->
  (_, _, Ocapi.Endpoint.complete) Ocapi.Endpoint.t ->
  Yojson.Safe.t
(** The responses of one status are documented from the endpoint's cases first, then from what
    its providers declare, then from [common]. *)

val schemas : Endpoints.t -> (string * Yojson.Safe.t) list
(** The named schemas the endpoints refer to in their request bodies and responses, in order of
    appearance, without duplicates. Hidden endpoints are left out. [Invalid_argument] when one
    name is declared with two different schemas. *)

val paths : ?common:Ocapi.Endpoint.common list -> Endpoints.t -> (string * Yojson.Safe.t) list
(** One path item per path template; endpoints with the same template share an item. Hidden
    endpoints are left out. *)

val document :
  ?servers:Yojson.Safe.t list ->
  ?tags:Yojson.Safe.t list ->
  ?security_schemes:(string * Yojson.Safe.t) list ->
  ?common:Ocapi.Endpoint.common list ->
  ?schemas:(string * Yojson.Safe.t) list ->
  info:Yojson.Safe.t ->
  Endpoints.t ->
  Yojson.Safe.t
(** An OpenAPI 3.1 document: [info], {!paths} and, under [components], {!val-schemas} and the
    [schemas] given (snake_case names, shared definitions the others refer to, say) by their
    {!component_name}, and the [security_schemes] as given. *)
