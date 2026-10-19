let bearer_token value =
  match String.index_opt value ' ' with
  | None -> None
  | Some i ->
    let scheme = String.sub value 0 i in
    if String.lowercase_ascii scheme <> "bearer" then
      None
    else
      let token = String.sub value (i + 1) (String.length value - i - 1) |> String.trim in
      if token = "" then None else Some token

let credential ~scheme get =
  Ocapi.Endpoint.Provide.make ~security:[ scheme ] ~encode:(fun q ->
    {
      Ocapi.Endpoint.Encoded.empty with
      headers =
        get q |> Option.fold ~none:[] ~some:(fun token -> [ "authorization", "Bearer " ^ token ]);
    })
  @@ fun request -> Ok (Option.bind (request.header "authorization") bearer_token)

module Make (User : sig
  type t

  val scheme : string
end) =
struct
  type _ Effect.t += Current : User.t Effect.t

  let current () = Effect.perform Current

  let required () =
    Ocapi.Endpoint.Provide.make ~security:[ User.scheme ] ~responses:[ 401, "Not authenticated" ]
    @@ fun _ -> Ok (ignore (current ()))

  let run (user : User.t) f = try f () with effect Current, k -> Effect.Deep.continue k user
end
