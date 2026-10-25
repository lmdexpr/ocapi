type route = Route : ('a, 'r, _) Endpoint.t * ('a -> 'r) -> route

let handle f endpoint = Route (endpoint, f)

let run (Route (endpoint, f)) (request : Endpoint.Request.t) params =
  match Endpoint.resolve endpoint { request with params } with
  | Error reply -> reply
  | Ok a -> Endpoint.reply endpoint (f a)

let dispatch routes (request : Endpoint.Request.t) =
  routes
  |> List.find_map @@ fun (Route (endpoint, _) as route) ->
     if Endpoint.meth endpoint <> request.meth then
       None
     else
       Endpoint.params endpoint request.segments |> Option.map (run route request)

let serve ?(not_found = Endpoint.Reply.empty 404) routes request =
  dispatch routes request |> Option.value ~default:not_found
