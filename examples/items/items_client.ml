let list ?limit () = Ocapi.Client.invoke (module Items_api.Index) { limit }
let get id = Ocapi.Client.invoke (module Items_api.Get) { id }
let create name = Ocapi.Client.invoke (module Items_api.Create) { name }
