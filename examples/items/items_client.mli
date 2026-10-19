(** Calls to {!Items_api}, one line each: the endpoint decodes its own responses. Each performs
    [Ocapi.Client.Send]: run them under a handler such as [Ocapi_cohttp_eio.Client.run]. *)

val list : ?limit:int -> unit -> (Items_api.item list, Ocapi.Client.error) result
val get : string -> ((Items_api.item, string) result, Ocapi.Client.error) result
val create : string -> (Items_api.item, Ocapi.Client.error) result
