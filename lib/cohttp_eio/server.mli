(** Serves {!Ocapi.Endpoint} requests with cohttp-eio.

    Besides translating requests and replies, the accept loop keeps reused connections in sync,
    which cohttp-eio alone does not: it reads the next request from the same connection as soon as
    the handler returns, whether or not the body was consumed, and it accepts framing that leaves
    body bytes in the connection. Behind a load balancer that reuses connections, those bytes
    would be read as the start of another user's request. *)

type callback =
  Cohttp_eio.Server.conn -> Http.Request.t -> Cohttp_eio.Body.t -> Cohttp_eio.Server.response

exception Connection_closed of string
(** Raised after a response is written when the connection must not serve another request. It
    reaches the [on_connection_error] of {!run}. *)

val draining : ?max_unread:int -> callback -> callback
(** Checks the framing of the request before calling [callback]: a body on a method without one
    (GET, HEAD; [Content-Length: 0] is fine), a Transfer-Encoding other than [chunked], both Content-Length and
    Transfer-Encoding, or an invalid or repeated Content-Length is answered with an empty 400 and
    {!Connection_closed}. A request with neither header has an empty body (RFC 9112 §6.3).

    After the response of [callback] is written, reads the rest of the request body and discards
    it, up to [max_unread] bytes (default 1 MB); beyond that, {!Connection_closed}. A body read to
    its end is not read again (the chunked reader would otherwise consume the next request
    line). *)

val request :
  ?max_body_size:int -> Http.Request.t -> Cohttp_eio.Body.t -> Ocapi.Endpoint.Request.t option
(** [None] for a method endpoints do not have ([HEAD], [OPTIONS], ...). Path segments are matched
    after percent-decoding ([/%61dmin] is [/admin]). The body is read when the endpoint asks for
    it, up to [max_body_size] bytes (default 1 MB); [Eio.Buf_read.Buffer_limit_exceeded] beyond. *)

val respond : Ocapi.Endpoint.Reply.t -> Cohttp_eio.Server.response
(** A JSON body is sent with [Content-Type: application/json]. [Content-Length] and
    [Transfer-Encoding] from the reply are dropped: framing is cohttp's. A header name or value
    containing CR or LF is [Invalid_argument]. *)

val callback :
  ?max_body_size:int ->
  ?on_exception:(exn -> Ocapi.Endpoint.Reply.t) ->
  (Ocapi.Endpoint.Request.t -> Ocapi.Endpoint.Reply.t) ->
  callback
(** [callback serve]. A method without endpoints is a 404 and a body over [max_body_size] a 413
    (default 1 MB; parsing deeply nested JSON takes far more memory than its size, so raise it with
    care).
    Any other exception from [serve], or from turning its reply into a response, goes to
    [on_exception] (default: an empty 500); cancellation is not caught. Wrap it in {!draining}
    before giving it to cohttp-eio. *)

val run :
  net:_ Eio.Net.t ->
  ?addr:Eio.Net.Ipaddr.v4v6 ->
  port:int ->
  ?max_connections:int ->
  ?max_body_size:int ->
  ?max_unread:int ->
  ?on_exception:(exn -> Ocapi.Endpoint.Reply.t) ->
  ?on_connection_error:(exn -> unit) ->
  (Ocapi.Endpoint.Request.t -> Ocapi.Endpoint.Reply.t) ->
  'a
(** Serves [serve], typically [Ocapi.Server.serve routes], on [addr] (default: the IPv4 loopback;
    pass [Eio.Net.Ipaddr.V4.any] to listen on every address) with {!val-callback} inside
    {!draining}. Each request runs in the fiber of its connection, so an effect handler that
    [serve] relies on goes inside [serve], not around [run]. [on_connection_error] receives
    connection-level failures, {!Connection_closed} included, and ignores them by default.

    cohttp-eio puts no limit on the size or number of request headers and has no read or idle
    timeout, and this does not add them: a long header line costs CPU and memory quadratically.
    Run behind a proxy that limits headers and times out slow clients, and bound connections with
    [max_connections]. *)

val common : Ocapi.Endpoint.common list
(** The responses {!run} gives outside the endpoints, for [Ocapi_openapi.document
    ~common]: the 400 of {!draining}, the 413 of a body over the limit and the 500 of the default
    [on_exception]. *)
