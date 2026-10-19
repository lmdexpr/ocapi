(* The server and the client of the example, talking over the loopback interface. *)

let unused_port env =
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen env#net ~sw ~backlog:1 ~reuse_addr:true (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  match Eio.Net.listening_addr socket with `Tcp (_, port) -> port | `Unix _ -> assert false

let with_items f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let port = unused_port env in
  let routes = Items_server.routes (Items_server.store ()) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Ocapi_cohttp_eio.Server.run ~net:env#net ~addr:Eio.Net.Ipaddr.V4.loopback ~port
      (Ocapi.Server.serve routes));
  let client = Cohttp_eio.Client.make ~https:None env#net in
  let base = Uri.make ~scheme:"http" ~host:"127.0.0.1" ~port () in
  Ocapi_cohttp_eio.Client.run ~client ~base f

let item =
  Alcotest.testable (fun fmt (i : Items_api.item) -> Format.fprintf fmt "%s %s" i.id i.name) ( = )

let ok = function Ok v -> v | Error _ -> Alcotest.fail "expected Ok"

let test_create_list_get () =
  with_items @@ fun () ->
  let apple = Items_client.create "apple" |> ok in
  let pear = Items_client.create "pear" |> ok in
  Alcotest.(check (list item)) "in order" [ apple; pear ] (Items_client.list () |> ok);
  Alcotest.(check (list item)) "limited" [ apple ] (Items_client.list ~limit:1 () |> ok);
  Alcotest.(check (result item string)) "found" (Ok pear) (Items_client.get pear.id |> ok);
  Alcotest.(check (result item string))
    "missing" (Error "not_found")
    (Items_client.get "item-9" |> ok)

(* What the client refuses never reaches the server; what the server refuses comes back as is. *)
let test_rejections () =
  with_items @@ fun () ->
  (match Items_client.get "../admin" with
  | Error (`Invalid _) -> ()
  | Ok _ | Error _ -> Alcotest.fail "expected Invalid");
  (match Items_client.list ~limit:0 () with
  | Error (`Invalid _) -> ()
  | Ok _ | Error _ -> Alcotest.fail "expected Invalid");
  (* Sent anyway, by hand: the server refuses it as the contract says. *)
  let response =
    Ocapi.Client.(request ~query:[ "limit", [ "0" ] ] (target Items_api.Index.endpoint) |> send)
  in
  Alcotest.(check (pair int string))
    "400"
    (400, {|{"error":"invalid_limit"}|})
    (response.status, response.body)

let () =
  Alcotest.run "items"
    [
      ( "client and server",
        [
          Alcotest.test_case "create, list, get" `Quick test_create_list_get;
          Alcotest.test_case "rejections" `Quick test_rejections;
        ] );
    ]
