(** The contract of a small item store, shared by its server and its client. Each endpoint has
    its request as a record. *)

type item = { id : string; name : string }

(** [GET /items?limit=n]: the items in the order they were created, at most [n] of them. *)
module Index : sig
  type request = { limit : int option }

  val endpoint : (request, item list, Ocapi.Endpoint.complete) Ocapi.Endpoint.t
end

(** [GET /items/{id}]. *)
module Get : sig
  type request = { id : string }

  val endpoint : (request, (item, string) result, Ocapi.Endpoint.complete) Ocapi.Endpoint.t
end

(** [POST /items] with [{"name": ...}]: the created item, with an id the server chooses. *)
module Create : sig
  type request = { name : string }

  val endpoint : (request, item, Ocapi.Endpoint.complete) Ocapi.Endpoint.t
end
