let () =
  Eio_main.run @@ fun env ->
  let routes = Items_server.routes (Items_server.store ()) in
  Ocapi_cohttp_eio.Server.run ~net:env#net ~port:8080 (Ocapi.Server.serve routes)
