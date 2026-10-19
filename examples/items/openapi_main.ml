let () =
  Ocapi.Endpoint.Openapi.document
    ~info:(`Assoc [ "title", `String "Items"; "version", `String "1.0.0" ])
    ~common:Ocapi_cohttp_eio.Server.common Items_api.endpoints
  |> Yojson.Safe.pretty_to_string |> print_endline
