let attempt ~clock ~timeout request =
  match timeout with
  | None -> Ok (Ocapi.Client.send request)
  | Some seconds -> (
    match Eio.Time.with_timeout_exn clock seconds (fun () -> Ocapi.Client.send request) with
    | response -> Ok response
    | exception Eio.Time.Timeout -> Error Eio.Time.Timeout)

let retryable (response : Ocapi.Client.response) = response.status >= 500

let rec go ~clock ~timeout ~left request =
  match attempt ~clock ~timeout request with
  | Ok response when retryable response && left > 1 -> go ~clock ~timeout ~left:(left - 1) request
  | Error _ when left > 1 -> go ~clock ~timeout ~left:(left - 1) request
  | (Ok _ | Error _) as last -> last

(* The handler clause runs outside this handler, so performing Send there reaches the transport. *)
(* Sending POST or PATCH again may apply it twice. *)
let idempotent = function `GET | `PUT | `DELETE -> true | `POST | `PATCH -> false

let run ~clock ?(attempts = 3) ?timeout f =
  try f ()
  with effect Ocapi.Client.Send request, k -> (
    let left = if idempotent request.meth then attempts else 1 in
    match go ~clock ~timeout ~left request with
    | Ok response -> Effect.Deep.continue k response
    | Error exn -> Effect.Deep.discontinue k exn
    | exception exn -> Effect.Deep.discontinue_with_backtrace k exn (Printexc.get_raw_backtrace ()))
