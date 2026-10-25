type target = { meth : Endpoint.meth; path : string }

let segment endpoint params segment =
  let n = String.length segment in
  if n >= 2 && segment.[0] = '{' && segment.[n - 1] = '}' then
    let name = String.sub segment 1 (n - 2) in
    match List.assoc_opt name params with
    | Some value when Endpoint.Path.valid value -> Ok value
    | Some value ->
      Error (Printf.sprintf "%s must contain only letters, digits, '_' and '-', got %S" name value)
    | None -> invalid_arg ("no value for " ^ segment ^ " in " ^ Endpoint.template endpoint)
  else
    Ok segment

let fill endpoint params =
  Endpoint.template endpoint |> String.split_on_char '/'
  |> List.fold_left
       (fun acc s ->
         Result.bind acc @@ fun filled ->
         segment endpoint params s |> Result.map @@ fun s -> s :: filled)
       (Ok [])
  |> Result.map @@ fun filled ->
     { meth = Endpoint.meth endpoint; path = List.rev filled |> String.concat "/" }

(* Without values, a name that cannot be filled is [Invalid_argument]; [Error] is unreachable. *)
let target endpoint = fill endpoint [] |> Result.fold ~ok:Fun.id ~error:invalid_arg

type request = {
  meth : Endpoint.meth;
  path : string;
  query : (string * string list) list;
  headers : (string * string) list;
  body : string option;
}

type response = { status : int; headers : (string * string) list; body : string }
type _ Effect.t += Send : request -> response Effect.t

let header name headers =
  let name = String.lowercase_ascii name in
  headers
  |> List.find_map @@ fun (key, value) ->
     if String.lowercase_ascii key = name then Some value else None

let request ?(query = []) ?bearer ?(headers = []) ?body ({ meth; path } : target) =
  let content_type =
    match body, header "content-type" headers with
    | Some _, None -> [ "content-type", "application/json" ]
    | Some _, Some _ | None, (Some _ | None) -> []
  in
  let headers =
    Option.fold bearer ~none:[] ~some:(fun token -> [ "authorization", "Bearer " ^ token ])
    @ headers @ content_type
  in
  { meth; path; query; headers; body }

let send request = Effect.perform (Send request)

type error =
  [ `Invalid of string | `Unauthorized | `Undeclared of response | `Malformed of response * string ]

let one_line s = not (String.exists (function '\r' | '\n' | '\000' -> true | _ -> false) s)

(* A line break would end the header and start another (or the body): the value would choose
   headers of its own. *)
let checked_headers headers =
  match headers |> List.find_opt (fun (name, value) -> not (one_line name && one_line value)) with
  | Some (name, _) -> Error (Printf.sprintf "header %s contains a line break" name)
  | None -> (
    match
      headers |> List.filter (fun (name, _) -> String.lowercase_ascii name = "authorization")
    with
    | _ :: _ :: _ -> Error "authorization is given twice (?bearer and the request)"
    | [] | [ _ ] -> Ok headers)

let call ?bearer endpoint q =
  let request =
    Endpoint.encode endpoint q
    |> Fun.flip Result.bind @@ fun (encoded : Endpoint.Encoded.t) ->
       fill endpoint encoded.params
       |> Fun.flip Result.bind @@ fun target ->
          (* RFC 6265 §5.4: one Cookie header for all cookies. *)
          let cookie =
            match encoded.cookies with
            | [] -> []
            | cookies ->
              [ "cookie", cookies |> List.map (fun (n, v) -> n ^ "=" ^ v) |> String.concat "; " ]
          in
          let request =
            request ~query:encoded.query ?bearer ~headers:(encoded.headers @ cookie)
              ?body:encoded.body target
          in
          checked_headers request.headers |> Result.map @@ fun headers -> { request with headers }
  in
  match request with
  | Error reason -> Error (`Invalid reason)
  | Ok request -> (
    let response = send request in
    match
      Endpoint.decode endpoint ~status:response.status ~headers:response.headers response.body
    with
    | Ok r -> Ok r
    | Error (`Malformed reason) -> Error (`Malformed (response, reason))
    | Error `Undeclared when response.status = 401 -> Error `Unauthorized
    | Error `Undeclared -> Error (`Undeclared response))

let invoke (type q r) ?bearer (module E : Endpoint.S with type request = q and type result = r)
  (q : q) =
  call ?bearer E.endpoint q
