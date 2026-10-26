let list ?limit () = Ocapi.Client.invoke (module Items_api.Index) { limit }

let get id =
  Ocapi.Client.invoke (module Items_api.Get) { id }
  |> Result.map (function
    | Items_api.Get.Found item -> Ok item
    | Not_found { error } -> Error error)

let create name =
  Ocapi.Client.invoke (module Items_api.Create) { item = { name } }
  |> Result.map (fun (Items_api.Create.Created item) -> item)
