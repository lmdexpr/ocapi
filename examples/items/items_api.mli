(** The contract of a small item store, shared by its server and its client. Each endpoint is a
    module: its request as a record, its result, and the endpoint ({!Ocapi.Endpoint.S}). *)

type item = { id : string; name : string }

(** [GET /items?limit=n]: the items in the order they were created, at most [n] of them. *)
module Index : sig
  type request = { limit : int option }
  type result = item list

  include
    Ocapi.Endpoint.S
      with type request := request
       and type result := result
       and type doc = Ocapi.Endpoint.complete
end

(** [GET /items/{id}]. *)
module Get : sig
  type request = { id : string }
  type nonrec result = (item, string) result

  include
    Ocapi.Endpoint.S
      with type request := request
       and type result := result
       and type doc = Ocapi.Endpoint.complete
end

(** [POST /items] with [{"name": ...}]: the created item, with an id the server chooses. *)
module Create : sig
  type request = { name : string }
  type result = item

  include
    Ocapi.Endpoint.S
      with type request := request
       and type result := result
       and type doc = Ocapi.Endpoint.complete
end
