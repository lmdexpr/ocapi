(** Answers [Ocapi.Client.Send] by dispatching to routes in the same process. No socket is
    involved: the client and the server meet at the contract. *)

val run : Ocapi.Server.route list -> (unit -> 'a) -> 'a
