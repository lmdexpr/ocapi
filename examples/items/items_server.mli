(** The handlers of {!Items_api}, over an in-memory store. *)

type store

val store : unit -> store
val routes : store -> Ocapi.Server.route list
