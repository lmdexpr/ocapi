open Ocapi

let request (r : Client.request) : Endpoint.Request.t =
  {
    meth = r.meth;
    segments = String.split_on_char '/' r.path |> List.filter (fun s -> s <> "");
    query = r.query;
    header = (fun name -> Client.header name r.headers);
    body = (fun () -> Option.value r.body ~default:"");
    params = [];
  }

let response (reply : Endpoint.Reply.t) : Client.response =
  match reply.body with
  | Empty -> { status = reply.status; headers = reply.headers; body = "" }
  | Raw body -> { status = reply.status; headers = reply.headers; body }
  | Json json ->
    {
      status = reply.status;
      headers =
        (match Client.header "content-type" reply.headers with
        | Some _ -> reply.headers
        | None -> ("content-type", "application/json") :: reply.headers);
      body = Yojson.Safe.to_string json;
    }

let run routes f =
  try f ()
  with effect Client.Send r, k ->
    Effect.Deep.continue k (Server.serve routes (request r) |> response)
