(** Retries and times out [Ocapi.Client.Send] by performing it again towards the handler outside.
    Handlers stack: install this one inside a transport handler such as
    [Ocapi_cohttp_eio.Client.run]. *)

val run : clock:_ Eio.Time.clock -> ?attempts:int -> ?timeout:float -> (unit -> 'a) -> 'a
(** Each attempt is given [timeout] seconds (none by default). A 5xx response or a timeout of an
    idempotent request (GET, PUT, DELETE) is attempted again, up to [attempts] times (3 by
    default); the last response is then answered as it is, and a timeout on the last attempt raises
    [Eio.Time.Timeout] where the request was performed. POST and PATCH are attempted once. Any
    other exception is not retried. *)
