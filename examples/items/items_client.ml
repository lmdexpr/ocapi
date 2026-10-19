let list ?limit () = Ocapi.Client.call Items_api.Index.endpoint { limit }
let get id = Ocapi.Client.call Items_api.Get.endpoint { id }
let create name = Ocapi.Client.call Items_api.Create.endpoint { name }
