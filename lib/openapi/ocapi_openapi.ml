module Endpoint = Ocapi.Endpoint
module Meta = Endpoint.Meta

module Endpoints = struct
  type t = [] : t | ( :: ) : (_, _, Endpoint.complete) Endpoint.t * t -> t

  let rec append a b = match a with [] -> b | e :: rest -> e :: append rest b
end

let rec metas = function
  | Endpoints.[] -> []
  | Endpoints.(e :: rest) -> Meta.of_endpoint e :: metas rest

let pascal name =
  name |> String.split_on_char '_' |> List.map String.capitalize_ascii |> String.concat ""

(* A named schema is a component; an unnamed one is written where it is used. *)
let schema_ref ({ name; schema } : Meta.schema) =
  match name with
  | Some name -> `Assoc [ "$ref", `String ("#/components/schemas/" ^ pascal name) ]
  | None -> schema

let content ?(media = "application/json") schema = `Assoc [ media, `Assoc [ "schema", schema ] ]
let component_name = pascal
let optional key = Option.fold ~none:[] ~some:(fun v -> [ key, v ])
let described = Option.map (fun d -> `String d)

(* The description of a response is required: without one of the caller's, the reason phrase. *)
let reason_phrase status =
  match status with
  | 100 -> "Continue"
  | 101 -> "Switching Protocols"
  | 200 -> "OK"
  | 201 -> "Created"
  | 202 -> "Accepted"
  | 203 -> "Non-Authoritative Information"
  | 204 -> "No Content"
  | 205 -> "Reset Content"
  | 206 -> "Partial Content"
  | 300 -> "Multiple Choices"
  | 301 -> "Moved Permanently"
  | 302 -> "Found"
  | 303 -> "See Other"
  | 304 -> "Not Modified"
  | 307 -> "Temporary Redirect"
  | 308 -> "Permanent Redirect"
  | 400 -> "Bad Request"
  | 401 -> "Unauthorized"
  | 402 -> "Payment Required"
  | 403 -> "Forbidden"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | 406 -> "Not Acceptable"
  | 407 -> "Proxy Authentication Required"
  | 408 -> "Request Timeout"
  | 409 -> "Conflict"
  | 410 -> "Gone"
  | 411 -> "Length Required"
  | 412 -> "Precondition Failed"
  | 413 -> "Content Too Large"
  | 414 -> "URI Too Long"
  | 415 -> "Unsupported Media Type"
  | 416 -> "Range Not Satisfiable"
  | 417 -> "Expectation Failed"
  | 421 -> "Misdirected Request"
  | 422 -> "Unprocessable Content"
  | 425 -> "Too Early"
  | 426 -> "Upgrade Required"
  | 428 -> "Precondition Required"
  | 429 -> "Too Many Requests"
  | 431 -> "Request Header Fields Too Large"
  | 451 -> "Unavailable For Legal Reasons"
  | 500 -> "Internal Server Error"
  | 501 -> "Not Implemented"
  | 502 -> "Bad Gateway"
  | 503 -> "Service Unavailable"
  | 504 -> "Gateway Timeout"
  | 505 -> "HTTP Version Not Supported"
  | 511 -> "Network Authentication Required"
  | status -> "Status " ^ string_of_int status

let description status d = `String (Option.value d ~default:(reason_phrase status))

let parameter (p : Meta.parameter) =
  `Assoc
    ([
       "name", `String p.name;
       ( "in",
         `String
           (match p.in_ with
           | `Path -> "path"
           | `Query -> "query"
           | `Header -> "header"
           | `Cookie -> "cookie") );
       "required", `Bool p.required;
     ]
    @ optional "description" (described p.description)
    @ [ "schema", p.schema ]
    @ optional "example" p.example)

(* Path parameters first, in template order; then query parameters in provider order. *)
let parameters (endpoint : Meta.endpoint) =
  let path, query =
    endpoint.parameters |> List.partition (fun (p : Meta.parameter) -> p.in_ = `Path)
  in
  let position (p : Meta.parameter) =
    endpoint.segments
    |> List.mapi (fun i s -> i, s)
    |> List.find_map (fun (i, s) -> if s = `Param p.name then Some i else None)
  in
  List.sort (fun a b -> compare (position a) (position b)) path @ query |> List.map parameter

let headers_of ~endpoint_headers ?(cases = []) (endpoint : Meta.endpoint) =
  match
    (if endpoint_headers then
       endpoint.headers
       |> List.filter_map (fun (h : Endpoint.header) ->
         Option.map (fun d -> h.name, d) h.description)
     else
       [])
    @ List.concat_map (fun (c : Meta.case) -> c.headers) cases
    |> List.fold_left
         (fun acc (name, d) -> if List.mem_assoc name acc then acc else acc @ [ name, d ])
         []
  with
  | [] -> []
  | documented ->
    [
      ( "headers",
        `Assoc
          (documented
          |> List.map @@ fun (name, description) ->
             ( name,
               `Assoc
                 [
                   "description", `String description; "schema", `Assoc [ "type", `String "string" ];
                 ] )) );
    ]

(* What the handler's cases declare wins over what the providers declare, which wins over what
   the transport declares: the more specific source of one status. Its description is the first
   one written, in the same order, so that a case or provider left undescribed does not hide the
   description another one gives. *)
let responses ~(common : Endpoint.common list) (endpoint : Meta.endpoint) =
  let declared =
    endpoint.cases |> List.map (fun (c : Meta.case) -> c.status) |> List.sort_uniq compare
  in
  let transport_all =
    common
    |> List.filter_map (fun (c : Endpoint.common) ->
      match c.scope with
      | `Every_operation -> Some (c.status, Some c.description)
      | `Operations_with_body ->
        Option.map (fun _ -> c.status, Some c.description) endpoint.request_body)
  in
  let descriptions =
    List.map (fun (c : Meta.case) -> c.status, c.description) endpoint.cases
    @ endpoint.responses @ transport_all
  in
  let description_of status =
    descriptions
    |> List.find_map (fun (s, d) -> if s = status then d else None)
    |> description status
  in
  let unless_known known statuses =
    statuses
    |> List.fold_left
         (fun acc status ->
           if List.mem status known || List.mem status acc then acc else acc @ [ status ])
         []
  in
  let provided = endpoint.responses |> List.map fst |> unless_known declared in
  let transport = transport_all |> List.map fst |> unless_known (declared @ provided) in
  let described ~headers status =
    let content =
      List.assoc_opt status endpoint.rejections
      |> Option.fold ~none:[] ~some:(fun named -> [ "content", content (schema_ref named) ])
    in
    ( status,
      ( string_of_int status,
        `Assoc
          ((("description", description_of status) :: headers_of ~endpoint_headers:headers endpoint)
          @ content) ) )
  in
  (* Several cases with one status: the schemas as a oneOf in declaration order. *)
  let document status =
    let cases = endpoint.cases |> List.filter (fun (c : Meta.case) -> c.status = status) in
    let schemas =
      cases
      |> List.filter_map (fun (c : Meta.case) -> Option.map schema_ref c.content)
      |> List.fold_left (fun acc s -> if List.mem s acc then acc else acc @ [ s ]) []
    in
    let content =
      match schemas with
      | [] -> []
      | [ schema ] -> [ "content", content schema ]
      | schemas -> [ "content", content (`Assoc [ "oneOf", `List schemas ]) ]
    in
    (* The endpoint headers go out through [reply] and [resolve] only: a status that only
       [outside] cases declare never carries them. *)
    let endpoint_headers = List.exists (fun (c : Meta.case) -> not c.outside) cases in
    let rest = headers_of ~endpoint_headers ~cases endpoint @ content in
    status, (string_of_int status, `Assoc (("description", description_of status) :: rest))
  in
  `Assoc
    (List.map document declared
     @ List.map (described ~headers:true) provided
     @ List.map (described ~headers:false) transport
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map snd)

let operation_of ~common (endpoint : Meta.endpoint) =
  (* Each provider's schemes are alternatives, and every provider must be satisfied: one
     requirement object per choice of a scheme from each. *)
  let security =
    let requirements =
      endpoint.security
      |> List.fold_left
           (fun choices alternatives ->
             choices
             |> List.concat_map @@ fun chosen ->
                alternatives
                |> List.map @@ fun scheme ->
                   if List.mem scheme chosen then chosen else chosen @ [ scheme ])
           [ [] ]
      |> List.sort_uniq compare
    in
    match requirements with
    | [ [] ] -> []
    | requirements ->
      [
        ( "security",
          `List
            (requirements
            |> List.map @@ fun schemes -> `Assoc (List.map (fun s -> s, `List []) schemes)) );
      ]
  in
  let request =
    endpoint.request_body
    |> Option.map (fun ({ schema; content_type } : Meta.body) ->
      `Assoc [ "required", `Bool true; "content", content ~media:content_type (schema_ref schema) ])
    |> optional "requestBody"
  in
  let parameters = match parameters endpoint with [] -> [] | ps -> [ "parameters", `List ps ] in
  let tags =
    match endpoint.tags with
    | [] -> []
    | tags -> [ "tags", `List (List.map (fun t -> `String t) tags) ]
  in
  `Assoc
    (tags
    @ optional "summary" (described endpoint.summary)
    @ optional "description" (described endpoint.description)
    @ security @ request @ parameters
    @ [ "responses", responses ~common endpoint ])

let operation ?(common = []) endpoint = operation_of ~common (Meta.of_endpoint endpoint)

(* One name with two schemas is a bug: stop rather than silently pick one. *)
(* Names meet as components names, so [foo_bar] and [foo__bar] are one name. *)
let add acc (name, schema) =
  match List.find_opt (fun (known, _) -> pascal known = pascal name) acc with
  | None -> acc @ [ name, schema ]
  | Some (_, known) when Yojson.Safe.equal known schema -> acc
  | Some (known, _) when known = name ->
    invalid_arg ("schema " ^ name ^ " is declared with two different schemas")
  | Some (known, _) ->
    invalid_arg
      ("schemas " ^ known ^ " and " ^ name ^ " are both " ^ pascal name
     ^ " in the components, with different schemas")

(* The endpoints are read into Meta once, at the entry points; what follows works on that. *)
let visible endpoints = metas endpoints |> List.filter (fun (e : Meta.endpoint) -> not e.hidden)

let schemas_of_metas metas =
  metas
  |> List.concat_map (fun (e : Meta.endpoint) ->
    (e.request_body |> Option.map (fun (b : Meta.body) -> b.schema) |> Option.to_list)
    @ List.map snd e.rejections
    @ List.filter_map (fun (c : Meta.case) -> c.content) e.cases)
  (* Unnamed schemas are written inline, not as components. *)
  |> List.filter_map (fun ({ name; schema } : Meta.schema) ->
    Option.map (fun name -> name, schema) name)
  |> List.fold_left add []

(* OpenAPI forbids two operations of one method on a path, and two paths that differ only in the
   names of their parameters. The router would also only ever reach the first. *)
let check_unique metas =
  let shape (e : Meta.endpoint) =
    e.segments |> List.map (function `Lit s -> s | `Param _ -> "{}")
  in
  metas
  |> List.fold_left
       (fun seen (e : Meta.endpoint) ->
         seen
         |> List.iter (fun (known : Meta.endpoint) ->
           if shape known = shape e && known.template <> e.template then
             invalid_arg ("paths " ^ known.template ^ " and " ^ e.template ^ " are the same path");
           if known.template = e.template && known.meth = e.meth then
             invalid_arg (Endpoint.meth_to_string e.meth ^ " " ^ e.template ^ " is declared twice"));
         e :: seen)
       []
  |> ignore

let paths_of_metas ~common metas =
  check_unique metas;
  let ops =
    metas
    |> List.map @@ fun (e : Meta.endpoint) ->
       e.template, (String.lowercase_ascii (Endpoint.meth_to_string e.meth), operation_of ~common e)
  in
  ops
  |> List.fold_left (fun acc (t, _) -> if List.mem t acc then acc else t :: acc) []
  |> List.rev
  |> List.map @@ fun t ->
     t, `Assoc (ops |> List.filter_map @@ fun (t', op) -> if t = t' then Some op else None)

let schemas endpoints = schemas_of_metas (visible endpoints)
let paths ?(common = []) endpoints = paths_of_metas ~common (visible endpoints)

let document ?servers ?tags ?security_schemes ?(common = []) ?(schemas = []) ~info endpoints =
  let metas = visible endpoints in
  let all = List.fold_left add (schemas_of_metas metas) schemas in
  let components =
    Option.fold security_schemes ~none:[] ~some:(fun s -> [ "securitySchemes", `Assoc s ])
    @ [ "schemas", `Assoc (all |> List.map @@ fun (name, schema) -> pascal name, schema) ]
  in
  `Assoc
    ([ "openapi", `String "3.1.0"; "info", info ]
    @ optional "servers" (Option.map (fun s -> `List s) servers)
    @ optional "tags" (Option.map (fun t -> `List t) tags)
    @ [ "paths", `Assoc (paths_of_metas ~common metas); "components", `Assoc components ])
