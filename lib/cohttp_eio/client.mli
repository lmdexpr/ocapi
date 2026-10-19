(** Handles {!Ocapi.Client.Send} with a cohttp-eio client. *)

val run : client:Cohttp_eio.Client.t -> base:Uri.t -> ?max_body_size:int -> (unit -> 'a) -> 'a
(** Sends the requests performed in [f] to [base]: the request path goes under the path of [base]
    and the request query after its query. TLS and proxies are the concern of [client]
    ([Cohttp_eio.Client.make ~https], [Cohttp_eio.Client.set_proxies]); the [https] wrapper is
    what verifies certificates.

    Effects do not cross fibers: [f] must perform {!Ocapi.Client.Send} in the fiber that called
    [run]. To call concurrently, call [run] inside each fiber.

    A transport failure raises where the request was performed, and so does [Invalid_argument]
    for a header name or value with a line break. So does
    [Eio.Buf_read.Buffer_limit_exceeded] for a response body over [max_body_size] bytes (default
    1 MB). Timeouts are the caller's: wrap the call in [Eio.Time.with_timeout]. *)
