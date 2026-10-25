(** Routing derived from endpoint values: a request in, a reply out. No transport is involved;
    an adapter such as [Ocapi_cohttp_eio.Server] builds the {!Endpoint.Request.t} and writes
    the {!Endpoint.Reply.t}. *)

type route

val handle : ('q -> 'r) -> ('q, 'r, _) Endpoint.t -> route
(** [endpoint |> handle (fun request -> ...)]. *)

val route : (module Endpoint.S with type request = 'q and type result = 'r) -> ('q -> 'r) -> route
(** {!handle} for an endpoint declared as a module: [route (module Get) (fun request -> ...)]. *)

val dispatch : route list -> Endpoint.Request.t -> Endpoint.Reply.t option
(** The reply of the first route whose method and path template match. [None] when none does.
    [Failure] when no response case matches the handler's result. *)

val serve : ?not_found:Endpoint.Reply.t -> route list -> Endpoint.Request.t -> Endpoint.Reply.t
(** {!dispatch}, answering [not_found] (default: an empty 404) when no route matches. Wrap the
    resulting function for anything that applies to every request (logging, authentication
    handlers, ...). *)
