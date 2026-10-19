open Ocapi

let target : Client.target = { meth = `GET; path = "/" }

(* A fake transport: answers from [script], one status per attempt, counting the attempts. As a
   real transport, a failure (here: being cancelled by the timeout) goes back through
   [discontinue]. *)
let transport ~clock ?(delay = 0.) script f =
  let count = ref 0 in
  let result =
    try f ()
    with effect Client.Send _, k -> (
      incr count;
      match Eio.Time.sleep clock delay with
      | () ->
        let status = List.nth script (min (!count - 1) (List.length script - 1)) in
        Effect.Deep.continue k ({ status; headers = []; body = "" } : Client.response)
      | exception exn -> Effect.Deep.discontinue k exn)
  in
  result, !count

let call () = Client.request target |> Client.send
let status (response : Client.response) = response.status

let test_a_5xx_is_retried () =
  Eio_main.run @@ fun env ->
  let clock = env#clock in
  let result, count =
    transport ~clock [ 503; 503; 200 ] @@ fun () ->
    Retry.run ~clock @@ fun () -> call ()
  in
  Alcotest.(check (pair int int)) "third attempt succeeds" (200, 3) (status result, count)

let test_the_last_response_is_answered_as_it_is () =
  Eio_main.run @@ fun env ->
  let clock = env#clock in
  let result, count =
    transport ~clock [ 503 ] @@ fun () ->
    Retry.run ~clock ~attempts:2 @@ fun () -> call ()
  in
  Alcotest.(check (pair int int)) "exhausted" (503, 2) (status result, count)

(* Sending a POST again may apply it twice. *)
let test_a_post_is_not_retried () =
  Eio_main.run @@ fun env ->
  let clock = env#clock in
  let result, count =
    transport ~clock [ 503; 200 ] @@ fun () ->
    Retry.run ~clock @@ fun () -> Client.request { meth = `POST; path = "/" } |> Client.send
  in
  Alcotest.(check (pair int int)) "once" (503, 1) (status result, count)

let test_a_4xx_is_not_retried () =
  Eio_main.run @@ fun env ->
  let clock = env#clock in
  let result, count =
    transport ~clock [ 404 ] @@ fun () ->
    Retry.run ~clock @@ fun () -> call ()
  in
  Alcotest.(check (pair int int)) "once" (404, 1) (status result, count)

let test_a_timeout_is_retried_then_raised () =
  Eio_main.run @@ fun env ->
  let clock = env#clock in
  let outcome, count =
    transport ~clock ~delay:0.2 [ 200 ] @@ fun () ->
    Retry.run ~clock ~attempts:2 ~timeout:0.01 @@ fun () ->
    match call () with _ -> "answered" | exception Eio.Time.Timeout -> "timeout"
  in
  Alcotest.(check (pair string int)) "raised where performed" ("timeout", 2) (outcome, count)

let () =
  Alcotest.run "retry"
    [
      ( "run",
        [
          Alcotest.test_case "a 5xx is retried" `Quick test_a_5xx_is_retried;
          Alcotest.test_case "the last response is answered as it is" `Quick
            test_the_last_response_is_answered_as_it_is;
          Alcotest.test_case "a 4xx is not retried" `Quick test_a_4xx_is_not_retried;
          Alcotest.test_case "a POST is not retried" `Quick test_a_post_is_not_retried;
          Alcotest.test_case "a timeout is retried, then raised" `Quick
            test_a_timeout_is_retried_then_raised;
        ] );
    ]
