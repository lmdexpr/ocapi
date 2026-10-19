(** Calls an endpoint and decodes its responses, both derived from the endpoint value. Sending is an
    effect: install a handler for {!Send} (e.g. [Ocapi_cohttp_eio.Client.run]) around the code
    that calls. The server never links this library; the two share only the endpoint.

    {!call} is what most code needs. {!val-target}, {!fill}, {!val-request} and {!send} build and send
    a request by hand, for a transport handler's tests, say. *)

type target = { meth : Endpoint.meth; path : string }

val target : _ Endpoint.t -> target
(** The target of an endpoint without path parameters. Passing an endpoint with path parameters
    is a bug of the caller: [Invalid_argument]. *)

val fill : _ Endpoint.t -> (string * string) list -> (target, string) result
(** The target with the [{name}] segments of the path template filled in. A value the server would
    not accept ({!Endpoint.Path.valid}) is not sent: [Error] with the reason. A name without a
    value is a bug of the caller: [Invalid_argument]. *)

type request = {
  meth : Endpoint.meth;
  path : string;
  query : (string * string list) list;
  headers : (string * string) list;
  body : string option;
}
(** [path] and [query] are relative to a base URL the {!Send} handler knows. *)

type response = { status : int; headers : (string * string) list; body : string }

type _ Effect.t +=
  | Send : request -> response Effect.t
      (** A handler answers with the response, whatever its status, and makes a transport failure raise
    where the request was performed (with [Effect.Deep.discontinue]). Effects do not cross fibers:
    the handler must be installed in the fiber that performs. *)

val request :
  ?query:(string * string list) list ->
  ?bearer:string ->
  ?headers:(string * string) list ->
  ?body:string ->
  target ->
  request
(** [bearer] goes into [Authorization: Bearer ...]; [body] is sent as [application/json] unless
    [headers] has a [Content-Type]. *)

val send : request -> response
(** Performs {!Send}. *)

type error =
  [ `Invalid of string | `Unauthorized | `Undeclared of response | `Malformed of response * string ]

val call : ?bearer:string -> ('q, 'r) Endpoint.t -> 'q -> ('r, [> error ]) result
(** Sends the request to the endpoint ({!Endpoint.encode}) and decodes the response into the
    handler's result with the endpoint's cases ({!Endpoint.decode}). Nothing is sent, and the
    result is [`Invalid], for a path or query value the server would not accept, a header name or value
    with a line break, or an [Authorization] given both by [bearer] and by the request. A 401 no case declares is
    [`Unauthorized]; any other status no case declares is [`Undeclared]; a body a case cannot
    decode is [`Malformed]. *)

val header : string -> (string * string) list -> string option
(** Case-insensitive lookup. *)
