let () =
  Ocapi_openapi.document
    ~info:(`Assoc [ "title", `String "Items"; "version", `String "1.0.0" ])
    ~common:Ocapi_cohttp_eio.Server.common
    [ Items_api.Index.endpoint; Items_api.Get.endpoint; Items_api.Create.endpoint ]
  |> Yojson.Safe.pretty_to_string |> print_endline
