let print (item : Items_api.item) = Printf.printf "%s %s\n" item.id item.name

let fail = function
  | `Invalid reason -> prerr_endline reason
  | `Unauthorized -> prerr_endline "unauthorized"
  | `Undeclared (response : Ocapi.Client.response) | `Malformed (response, _) ->
    Printf.eprintf "HTTP %d: %s\n" response.status response.body

let () =
  let base = if Array.length Sys.argv > 1 then Sys.argv.(1) else "http://127.0.0.1:8080" in
  Eio_main.run @@ fun env ->
  let client = Cohttp_eio.Client.make ~https:None env#net in
  Ocapi_cohttp_eio.Client.run ~client ~base:(Uri.of_string base) @@ fun () ->
  match Items_client.create "apple" with
  | Error e -> fail e
  | Ok created -> (
    print created;
    match Items_client.list () with Ok items -> List.iter print items | Error e -> fail e)
