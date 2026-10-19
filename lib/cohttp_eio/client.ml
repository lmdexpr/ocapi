let default_max_body_size = 1_000_000

(* The request path goes under the base path; the request query after the base query. *)
let uri ~base ({ path; query; _ } : Ocapi.Client.request) =
  let prefix =
    match Uri.path base with
    | "" | "/" -> ""
    | prefix when String.ends_with ~suffix:"/" prefix ->
      String.sub prefix 0 (String.length prefix - 1)
    | prefix -> prefix
  in
  let uri = Uri.with_path base (prefix ^ path) in
  match query with [] -> uri | query -> Uri.with_query uri (Uri.query uri @ query)

(* [Ocapi.Client.call] checks this already; requests built by hand reach here unchecked. *)
let one_line s = not (String.exists (function '\r' | '\n' | '\000' -> true | _ -> false) s)

let perform ~client ~base ~max_body_size (request : Ocapi.Client.request) =
  request.headers
  |> List.iter (fun (name, value) ->
    if not (one_line name && one_line value) then
      invalid_arg ("header " ^ name ^ " contains a line break"));
  Eio.Switch.run @@ fun sw ->
  let response, reader =
    Cohttp_eio.Client.call ~sw
      ~headers:(Http.Header.of_list request.headers)
      ?body:(Option.map Cohttp_eio.Body.of_string request.body)
      client
      (request.meth :> Http.Method.t)
      (uri ~base request)
  in
  Ocapi.Client.
    {
      status = Http.Status.to_int (Http.Response.status response);
      headers = Http.Header.to_list (Http.Response.headers response);
      body = Eio.Buf_read.of_flow ~max_size:max_body_size reader |> Eio.Buf_read.take_all;
    }

(* Raising from the handler frame would skip the handlers between the perform site and here, so
   failures go back through [discontinue]. *)
let run ~client ~base ?(max_body_size = default_max_body_size) f =
  try f ()
  with effect Ocapi.Client.Send request, k -> (
    match perform ~client ~base ~max_body_size request with
    | response -> Effect.Deep.continue k response
    | exception exn -> Effect.Deep.discontinue_with_backtrace k exn (Printexc.get_raw_backtrace ()))
