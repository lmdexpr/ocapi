type meth = [ `GET | `POST | `PUT | `PATCH | `DELETE ]

let meth_to_string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `PATCH -> "PATCH"
  | `DELETE -> "DELETE"

module Codec = struct
  type 'a t = {
    name : string;
    schema : Yojson.Safe.t;
    to_json : 'a -> Yojson.Safe.t;
    of_json : Yojson.Safe.t -> 'a;
  }

  let make ~name ~schema to_json of_json = { name; schema; to_json; of_json }
end

module Prism = struct
  type ('r, 'a) t = { inject : 'a -> 'r; project : 'r -> 'a option }

  let make ~inject ~project = { inject; project }
  let id = { inject = Fun.id; project = Option.some }
  let ok = { inject = Result.ok; project = Result.to_option }
  let error = { inject = Result.error; project = (function Error e -> Some e | Ok _ -> None) }
  let some = { inject = Option.some; project = Fun.id }
  let none = { inject = (fun () -> None); project = (function None -> Some () | Some _ -> None) }
end

module Request = struct
  type t = {
    meth : meth;
    segments : string list;
    query : (string * string list) list;
    header : string -> string option;
    body : unit -> string;
    params : (string * string) list;
  }
end

module Reply = struct
  type body = Empty | Json of Yojson.Safe.t | Raw of string
  type t = { status : int; headers : (string * string) list; body : body }

  let json ?(headers = []) status json = { status; headers; body = Json json }
  let empty status = { status; headers = []; body = Empty }
end

module Encoded = struct
  type t = {
    params : (string * string) list;
    query : (string * string list) list;
    headers : (string * string) list;
    cookies : (string * string) list;
    body : string option;
  }

  let empty = { params = []; query = []; headers = []; cookies = []; body = None }

  let append a b =
    {
      params = a.params @ b.params;
      query = a.query @ b.query;
      headers = a.headers @ b.headers;
      cookies = a.cookies @ b.cookies;
      body = (match a.body with Some _ as body -> body | None -> b.body);
    }
end

module Provide = struct
  type parameter = {
    name : string;
    in_ : [ `Path | `Query | `Header | `Cookie ];
    required : bool;
    description : string;
    schema : Yojson.Safe.t;
    example : Yojson.Safe.t option;
  }

  let parameter ~in_ ~required ?example ~description ~schema name =
    { name; in_; required; description; schema; example }

  type body_unread
  type body_read

  (* ['i] and ['o] are the body's state before and after the provider; only the interface uses
     them. *)
  type body = { name : string; schema : Yojson.Safe.t; content_type : string }

  type ('q, 'a, 'i, 'o) t = {
    security : string list list;
    parameters : parameter list;
    request_body : body option;
    responses : (int * string) list;
    rejections : (int * (string * Yojson.Safe.t)) list;
    encode : 'q -> (Encoded.t, string) result;
    resolve : Request.t -> ('a, Reply.t) result;
  }

  let checked ?security ?(parameters = []) ?(responses = []) ~encode resolve =
    {
      security = Option.to_list security;
      parameters;
      request_body = None;
      responses;
      rejections = [];
      encode;
      resolve;
    }

  let make ?security ?parameters ?responses ?(encode = Fun.const Encoded.empty) resolve =
    checked ?security ?parameters ?responses ~encode:(fun q -> Ok (encode q)) resolve

  let return a = make (fun _ -> Ok a)
  let map f t = { t with resolve = (fun request -> t.resolve request |> Result.map f) }
  let contramap f t = { t with encode = (fun q -> t.encode (f q)) }

  let both a b =
    {
      security = a.security @ b.security;
      parameters = a.parameters @ b.parameters;
      (* The interface lets at most one of them have a body. *)
      request_body = (match a.request_body with Some _ as r -> r | None -> b.request_body);
      responses = a.responses @ b.responses;
      rejections = a.rejections @ b.rejections;
      encode =
        (fun q -> Result.bind (a.encode q) @@ fun x -> b.encode q |> Result.map @@ Encoded.append x);
      resolve =
        (fun request ->
          match a.resolve request with
          | Error _ as e -> e
          | Ok x -> b.resolve request |> Result.map @@ fun y -> x, y);
    }

  let require ?(responses = []) on_error t =
    {
      t with
      responses = t.responses @ responses;
      resolve =
        (fun request -> t.resolve request |> Fun.flip Result.bind @@ Result.map_error on_error);
    }

  let require_json ~status ~description (codec : _ Codec.t) reject t =
    let t =
      require
        ~responses:[ status, description ]
        (fun e -> Reply.json status (codec.to_json (reject e)))
        t
    in
    { t with rejections = t.rejections @ [ status, (codec.name, codec.schema) ] }

  let resolve t request = t.resolve request
  let encode t q = t.encode q
end

module Syntax = struct
  let ( let+ ) t f = Provide.map f t
  let ( and+ ) = Provide.both
end

module Value = struct
  type 'a t = { schema : Yojson.Safe.t; parse : string -> 'a option; print : 'a -> string }

  let make ~schema parse print = { schema; parse; print }

  (* What the server would reject is not sent: it would come back as an error the contract does
     not declare. *)
  let printed value name v =
    let s = value.print v in
    match value.parse s with
    | Some _ -> Ok s
    | None -> Error (Printf.sprintf "%s: the server does not accept %S" name s)

  let string = make ~schema:(`Assoc [ "type", `String "string" ]) Option.some Fun.id

  let int_schema ?minimum ?maximum () =
    let bound key = Option.fold ~none:[] ~some:(fun n -> [ key, `Int n ]) in
    `Assoc ((("type", `String "integer") :: bound "minimum" minimum) @ bound "maximum" maximum)

  (* Decimal only: [int_of_string] also takes [0x10], [0b11], [1_000] and [+5], which a JSON
     Schema [integer] does not describe. *)
  let decimal s =
    let digits =
      match s with "" -> "" | s when s.[0] = '-' -> String.sub s 1 (String.length s - 1) | s -> s
    in
    if digits <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) digits then
      int_of_string_opt s
    else
      None

  let int ?minimum ?maximum () =
    let above n = Option.fold minimum ~none:true ~some:(fun m -> n >= m) in
    let below n = Option.fold maximum ~none:true ~some:(fun m -> n <= m) in
    make ~schema:(int_schema ?minimum ?maximum ())
      (fun s ->
        match decimal s with Some n when above n && below n -> Some n | Some _ | None -> None)
      string_of_int

  let bool =
    make
      ~schema:(`Assoc [ "type", `String "boolean" ])
      (function "true" -> Some true | "false" -> Some false | _ -> None)
      string_of_bool
end

module Path = struct
  (* An allow-list: a deny-list leaves room for the interpretation to shift with the order of
     decoding and dot-segment normalisation. *)
  let valid value =
    value <> ""
    && String.for_all
         (function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true | _ -> false)
         value

  let value ?(description = "") (value : _ Value.t) name get =
    let parameter =
      Provide.parameter ~in_:`Path ~required:true ~description ~schema:value.schema name
    in
    Provide.checked ~parameters:[ parameter ] ~encode:(fun q ->
      Value.printed value name (get q)
      |> Result.map @@ fun s -> { Encoded.empty with params = [ name, s ] })
    @@ fun (request : Request.t) ->
    List.assoc_opt name request.params
    |> Fun.flip Option.bind value.parse
    |> Option.to_result ~none:(Reply.empty 404)

  let string ?description name get = value ?description Value.string name get
end

module Query = struct
  type error = [ `Missing | `Invalid ]

  (* As [Uri.get_query_param]: the values of the first occurrence of the key, joined with ','. *)
  let raw ~unique name query =
    match List.filter (fun (key, _) -> String.equal key name) query with
    | [] -> Ok None
    | [ (_, values) ] -> Ok (Some (String.concat "," values))
    | (_, values) :: _ :: _ ->
      if unique then Error `Invalid else Ok (Some (String.concat "," values))

  let param ~required ?example ~description (value : _ Value.t) name ~encode decode =
    let parameter =
      Provide.parameter ~in_:`Query ~required ?example ~description ~schema:value.schema name
    in
    Provide.checked ~parameters:[ parameter ] ~encode:(fun q ->
      encode q |> Result.map @@ fun query -> { Encoded.empty with query })
    @@ fun (request : Request.t) -> Ok (decode request.query)

  let parse (value : _ Value.t) s = value.parse s |> Option.to_result ~none:`Invalid

  let required ?(unique = false) ?example ~description value name get =
    param ~required:true ?example ~description value name ~encode:(fun q ->
      Value.printed value name (get q) |> Result.map @@ fun s -> [ name, [ s ] ])
    @@ fun query ->
    match raw ~unique name query with
    | Error _ as e -> e
    | Ok None -> Error `Missing
    | Ok (Some s) -> parse value s

  let optional ?(unique = false) ?example ~description value name get =
    param ~required:false ?example ~description value name ~encode:(fun q ->
      match get q with
      | None -> Ok []
      | Some v -> Value.printed value name v |> Result.map @@ fun s -> [ name, [ s ] ])
    @@ fun query ->
    match raw ~unique name query with
    | Error _ as e -> e
    | Ok None -> Ok None
    | Ok (Some s) -> parse value s |> Result.map Option.some
end

module Header = struct
  type error = Query.error

  (* OpenAPI ignores header parameters of these names: Authorization belongs to the security
     schemes, the others to the media types. *)
  let reserved = [ "authorization"; "content-type"; "accept" ]

  let param ~required ?example ~description (value : _ Value.t) name ~encode decode =
    if List.mem (String.lowercase_ascii name) reserved then
      invalid_arg ("the header " ^ name ^ " is not a parameter; declare it as security or media");
    let parameter =
      Provide.parameter ~in_:`Header ~required ?example ~description ~schema:value.schema name
    in
    Provide.checked ~parameters:[ parameter ] ~encode:(fun q ->
      encode q |> Result.map @@ fun headers -> { Encoded.empty with headers })
    @@ fun (request : Request.t) -> Ok (decode (request.header name))

  let parse (value : _ Value.t) s = value.parse s |> Option.to_result ~none:`Invalid

  let required ?example ~description value name get =
    param ~required:true ?example ~description value name ~encode:(fun q ->
      Value.printed value name (get q) |> Result.map @@ fun s -> [ name, s ])
    @@ function
    | None -> Error `Missing
    | Some s -> parse value s

  let optional ?example ~description value name get =
    param ~required:false ?example ~description value name ~encode:(fun q ->
      match get q with
      | None -> Ok []
      | Some v -> Value.printed value name v |> Result.map @@ fun s -> [ name, s ])
    @@ function
    | None -> Ok None
    | Some s -> parse value s |> Result.map Option.some
end

module Cookie = struct
  type error = Query.error

  (* RFC 6265 §4.1.1: a value with anything else would end the cookie or the header. *)
  let octet = function
    | '\x21' | '\x23' .. '\x2B' | '\x2D' .. '\x3A' | '\x3C' .. '\x5B' | '\x5D' .. '\x7E' -> true
    | _ -> false

  let printed value name v =
    Value.printed value name v
    |> Fun.flip Result.bind @@ fun s ->
       if String.for_all octet s then
         Ok s
       else
         Error (Printf.sprintf "%s: %S is not a cookie value" name s)

  let find name header =
    header
    |> Option.map (String.split_on_char ';')
    |> Option.value ~default:[]
    |> List.find_map @@ fun pair ->
       match String.index_opt pair '=' with
       | Some i when String.trim (String.sub pair 0 i) = name ->
         Some (String.trim (String.sub pair (i + 1) (String.length pair - i - 1)))
       | Some _ | None -> None

  let param ~required ?example ~description (value : _ Value.t) name ~encode decode =
    let parameter =
      Provide.parameter ~in_:`Cookie ~required ?example ~description ~schema:value.schema name
    in
    Provide.checked ~parameters:[ parameter ] ~encode:(fun q ->
      encode q |> Result.map @@ fun cookies -> { Encoded.empty with cookies })
    @@ fun (request : Request.t) -> Ok (decode (find name (request.header "cookie")))

  let parse (value : _ Value.t) s = value.parse s |> Option.to_result ~none:`Invalid

  let required ?example ~description value name get =
    param ~required:true ?example ~description value name ~encode:(fun q ->
      printed value name (get q) |> Result.map @@ fun s -> [ name, s ])
    @@ function
    | None -> Error `Missing
    | Some s -> parse value s

  let optional ?example ~description value name get =
    param ~required:false ?example ~description value name ~encode:(fun q ->
      match get q with
      | None -> Ok []
      | Some v -> printed value name v |> Result.map @@ fun s -> [ name, s ])
    @@ function
    | None -> Ok None
    | Some s -> parse value s |> Result.map Option.some
end

module Body = struct
  let provider ~name ~schema ~content_type ~encode resolve =
    {
      (Provide.make
         ~encode:(fun q ->
           { Encoded.empty with headers = [ "content-type", content_type ]; body = Some (encode q) })
         resolve)
      with
      request_body = Some { name; schema; content_type };
    }

  let json (codec : _ Codec.t) get =
    provider ~name:codec.name ~schema:codec.schema ~content_type:"application/json"
      ~encode:(fun q -> Yojson.Safe.to_string (codec.to_json (get q)))
    @@ fun (request : Request.t) ->
    (* Reading the body is the adapter's; its failures (limits, cancellation) must propagate.
       [of_json] is the caller's codec; which exceptions it raises cannot be known here. *)
    let body = request.body () in
    match codec.of_json (Yojson.Safe.from_string body) with
    | value -> Ok (Ok value)
    | exception exn -> Ok (Error (`Malformed (Printexc.to_string exn)))

  let raw ~name ~schema ~content_type get =
    provider ~name ~schema ~content_type ~encode:get @@ fun (request : Request.t) ->
    Ok (request.body ())
end

module Response = struct
  type content = No_content | Schema of string * Yojson.Safe.t

  type 'r produced = {
    content : content;
    select : 'r -> ((string * string) list * Reply.body) option;
    decode : (string * string) list -> string -> ('r, string) result option;
    documented_headers : (string * string) list;
  }

  type 'r origin = Produced of 'r produced | Outside
  type 'r case = { status : int; description : string; origin : 'r origin }

  (* The caller's decoder; which exceptions it raises cannot be known here. *)
  let decoded of_json body =
    match of_json (Yojson.Safe.from_string body) with
    | value -> Ok value
    | exception exn -> Error (Printexc.to_string exn)

  let empty ~status ~description (prism : (_, unit) Prism.t) =
    {
      status;
      description;
      origin =
        Produced
          {
            content = No_content;
            select = (fun r -> prism.project r |> Option.map @@ fun () -> [], Reply.Empty);
            (* Only an empty body: a case of the same status with a body must get the chance
               to decode it. *)
            documented_headers = [];
            decode = (fun _ body -> if body = "" then Some (Ok (prism.inject ())) else None);
          };
    }

  let json ~status ~description (codec : _ Codec.t) (prism : _ Prism.t) =
    {
      status;
      description;
      origin =
        Produced
          {
            content = Schema (codec.name, codec.schema);
            select =
              (fun r -> prism.project r |> Option.map @@ fun a -> [], Reply.Json (codec.to_json a));
            documented_headers = [];
            decode = (fun _ body -> Some (decoded codec.of_json body |> Result.map prism.inject));
          };
    }

  let json_with_headers ~status ~description ?(headers = []) (codec : _ Codec.t) (prism : _ Prism.t)
      =
    {
      status;
      description;
      origin =
        Produced
          {
            content = Schema (codec.name, codec.schema);
            select =
              (fun r ->
                prism.project r |> Option.map @@ fun (sent, a) -> sent, Reply.Json (codec.to_json a));
            decode =
              (fun received body ->
                Some
                  (decoded codec.of_json body |> Result.map @@ fun a -> prism.inject (received, a)));
            documented_headers = headers;
          };
    }

  let unspecified_json ~status ~description (prism : _ Prism.t) =
    {
      status;
      description;
      origin =
        Produced
          {
            content = No_content;
            select = (fun r -> prism.project r |> Option.map @@ fun j -> [], Reply.Json j);
            documented_headers = [];
            decode = (fun _ body -> Some (decoded Fun.id body |> Result.map prism.inject));
          };
    }

  let raw ~status ~description (prism : _ Prism.t) =
    {
      status;
      description;
      origin =
        Produced
          {
            content = No_content;
            select =
              (fun r ->
                prism.project r |> Option.map @@ fun (headers, body) -> headers, Reply.Raw body);
            documented_headers = [];
            decode = (fun headers body -> Some (Ok (prism.inject (headers, body))));
          };
    }

  let outside ~status ~description = { status; description; origin = Outside }

  let errors ?(without_body = []) (codec : _ Codec.t) ~status (prism : _ Prism.t) documented =
    documented
    |> List.map @@ fun (code, description) ->
       let select r =
         Option.bind (prism.project r) (fun e -> if status e = code then Some e else None)
       in
       if List.mem code without_body then
         {
           status = code;
           description;
           origin =
             Produced
               {
                 content = No_content;
                 select = (fun r -> select r |> Option.map @@ fun _ -> [], Reply.Empty);
                 documented_headers = [];
                 decode = (fun _ _ -> None);
               };
         }
       else
         {
           status = code;
           description;
           origin =
             Produced
               {
                 content = Schema (codec.name, codec.schema);
                 select =
                   (fun r -> select r |> Option.map @@ fun e -> [], Reply.Json (codec.to_json e));
                 documented_headers = [];
                 decode =
                   (fun _ body ->
                     match decoded codec.of_json body with
                     | Ok e when status e = code -> Some (Ok (prism.inject e))
                     | Ok _ -> None
                     | Error _ as malformed -> Some malformed);
               };
         }
end

type segment = Lit of string | Param of string
type header = { name : string; value : string; description : string option }

type ('q, 'r) t = {
  meth : meth;
  template : string;
  segments : segment list;
  hidden : bool;
  headers : header list;
  tags : string list;
  summary : string;
  description : string option;
  provide : ('q, 'q, unit, unit) Provide.t;
  responses : 'r Response.case list;
}

let parse_template template =
  template |> String.split_on_char '/'
  |> List.filter (fun s -> s <> "")
  |> List.map @@ fun s ->
     let n = String.length s in
     if n >= 2 && s.[0] = '{' && s.[n - 1] = '}' then Param (String.sub s 1 (n - 2)) else Lit s

let make ~meth template ?(hidden = false) ?(headers = []) ~tags ~summary ?description provide
  ~responses =
  let segments = parse_template template in
  let in_template = segments |> List.filter_map (function Param p -> Some p | Lit _ -> None) in
  let provided =
    provide.Provide.parameters
    |> List.filter_map (fun (p : Provide.parameter) -> if p.in_ = `Path then Some p.name else None)
  in

  if not (String.starts_with ~prefix:"/" template) then
    invalid_arg ("the path template " ^ template ^ " does not start with /");

  if List.length (List.sort_uniq compare in_template) <> List.length in_template then
    invalid_arg ("the path template " ^ template ^ " names a parameter twice");

  if List.sort compare in_template <> List.sort compare provided then
    invalid_arg ("path parameters of " ^ template ^ " do not match its template");

  (* The body states are the interface's; the endpoint keeps the provider without them. *)
  let provide = { provide with Provide.resolve = provide.Provide.resolve } in

  if responses = [] then invalid_arg (template ^ " declares no response");

  { meth; template; segments; hidden; headers; tags; summary; description; provide; responses }

let meth t = t.meth
let template t = t.template

let params t path =
  let rec go segments path =
    match segments, path with
    | [], [] -> Some []
    | [], _ :: _ | _ :: _, [] -> None
    | Lit lit :: rest, x :: xs -> if String.equal lit x then go rest xs else None
    | Param name :: rest, x :: xs when Path.valid x ->
      go rest xs |> Option.map @@ fun ps -> (name, x) :: ps
    | Param _ :: _, _ :: _ -> None
  in
  go t.segments path

let with_headers t (reply : Reply.t) =
  { reply with headers = reply.headers @ List.map (fun (h : header) -> h.name, h.value) t.headers }

let resolve t request = Provide.resolve t.provide request |> Result.map_error (with_headers t)
let encode t q = Provide.encode t.provide q

let reply t r =
  match
    t.responses
    |> List.find_map @@ fun (c : _ Response.case) ->
       match c.origin with
       | Produced p ->
         p.select r
         |> Option.map @@ fun (headers, body) -> Reply.{ status = c.status; headers; body }
       | Outside -> None
  with
  | Some reply -> with_headers t reply
  | None -> failwith ("no response case matches the result of " ^ t.template)

(* The first case of the status that decodes wins; failing that, the first failure. *)
let decode t ~status ~headers body =
  let rec first = function
    | [] -> Error `Undeclared
    | Ok r :: _ -> Ok r
    | Error reason :: rest -> (
      match first rest with Ok r -> Ok r | Error _ -> Error (`Malformed reason))
  in
  t.responses
  |> List.filter_map (fun (c : _ Response.case) ->
    match c.origin with
    | Produced p when c.status = status -> p.decode headers body
    | Produced _ | Outside -> None)
  |> first

module Openapi = struct
  type ('a, 'r) endpoint = ('a, 'r) t

  module Endpoints = struct
    type t = [] : t | ( :: ) : (_, _) endpoint * t -> t

    let rec append a b = match a with [] -> b | e :: rest -> e :: append rest b
  end

  type packed = Packed : (_, _) t -> packed

  let rec packed = function Endpoints.[] -> [] | Endpoints.(e :: rest) -> Packed e :: packed rest

  let pascal name =
    name |> String.split_on_char '_' |> List.map String.capitalize_ascii |> String.concat ""

  let schema_ref name = `Assoc [ "$ref", `String ("#/components/schemas/" ^ pascal name) ]
  let content ?(media = "application/json") schema = `Assoc [ media, `Assoc [ "schema", schema ] ]
  let component_name = pascal

  let parameter (p : Provide.parameter) =
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
         "description", `String p.description;
         "schema", p.schema;
       ]
      @ Option.fold p.example ~none:[] ~some:(fun e -> [ "example", e ]))

  (* Path parameters first, in template order; then query parameters in provider order. *)
  let parameters endpoint =
    let path, query =
      endpoint.provide.parameters |> List.partition (fun (p : Provide.parameter) -> p.in_ = `Path)
    in
    let position (p : Provide.parameter) =
      endpoint.segments
      |> List.mapi (fun i s -> i, s)
      |> List.find_map (fun (i, s) -> if s = Param p.name then Some i else None)
    in
    List.sort (fun a b -> compare (position a) (position b)) path @ query |> List.map parameter

  let headers_of ~endpoint_headers ?(cases = []) endpoint =
    match
      (if endpoint_headers then
         endpoint.headers
         |> List.filter_map (fun (h : header) -> Option.map (fun d -> h.name, d) h.description)
       else
         [])
      @ List.concat_map
          (fun (c : _ Response.case) ->
            match c.origin with Produced p -> p.documented_headers | Outside -> [])
          cases
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
                     "description", `String description;
                     "schema", `Assoc [ "type", `String "string" ];
                   ] )) );
      ]

  type common = {
    status : int;
    description : string;
    scope : [ `Every_operation | `Operations_with_body ];
  }

  (* What the handler's cases declare wins over what the providers declare, which wins over what
     the transport declares: the more specific description of one status. *)
  let responses ~common endpoint =
    let declared =
      endpoint.responses
      |> List.map (fun (c : _ Response.case) -> c.status)
      |> List.sort_uniq compare
    in
    let unless_known known statuses =
      statuses
      |> List.fold_left
           (fun acc (status, description) ->
             if List.mem status known || List.mem_assoc status acc then
               acc
             else
               acc @ [ status, description ])
           []
    in
    let provided = endpoint.provide.responses |> unless_known declared in
    let transport =
      common
      |> List.filter_map (fun c ->
        match c.scope with
        | `Every_operation -> Some (c.status, c.description)
        | `Operations_with_body ->
          Option.map (fun _ -> c.status, c.description) endpoint.provide.request_body)
      |> unless_known (declared @ List.map fst provided)
    in
    let described ~headers (status, description) =
      let content =
        List.assoc_opt status endpoint.provide.rejections
        |> Option.fold ~none:[] ~some:(fun (name, _) -> [ "content", content (schema_ref name) ])
      in
      ( status,
        ( string_of_int status,
          `Assoc
            ((("description", `String description) :: headers_of ~endpoint_headers:headers endpoint)
            @ content) ) )
    in
    (* Several cases with one status: the description of the first, the schemas as a oneOf in
       declaration order. *)
    let document status =
      let cases =
        endpoint.responses |> List.filter (fun (c : _ Response.case) -> c.status = status)
      in
      let schemas =
        cases
        |> List.filter_map (fun (c : _ Response.case) ->
          match c.origin with
          | Produced { content = Schema (name, _); _ } -> Some (schema_ref name)
          | Produced { content = No_content; _ } | Outside -> None)
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
      let endpoint_headers =
        List.exists
          (fun (c : _ Response.case) -> match c.origin with Produced _ -> true | Outside -> false)
          cases
      in
      let rest = headers_of ~endpoint_headers ~cases endpoint @ content in
      let description = (List.hd cases).description in
      status, (string_of_int status, `Assoc (("description", `String description) :: rest))
    in
    `Assoc
      (List.map document declared
       @ List.map (described ~headers:true) provided
       @ List.map (described ~headers:false) transport
      |> List.sort (fun (a, _) (b, _) -> compare a b)
      |> List.map snd)

  let operation ?(common = []) endpoint =
    let optional key = Option.fold ~none:[] ~some:(fun v -> [ key, v ]) in
    (* Each provider's schemes are alternatives, and every provider must be satisfied: one
       requirement object per choice of a scheme from each. *)
    let security =
      let requirements =
        endpoint.provide.security
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
      endpoint.provide.request_body
      |> Option.map (fun ({ name; content_type; _ } : Provide.body) ->
        `Assoc [ "required", `Bool true; "content", content ~media:content_type (schema_ref name) ])
      |> optional "requestBody"
    in
    let parameters = match parameters endpoint with [] -> [] | ps -> [ "parameters", `List ps ] in
    `Assoc
      ([
         "tags", `List (endpoint.tags |> List.map @@ fun t -> `String t);
         "summary", `String endpoint.summary;
       ]
      @ (endpoint.description |> Option.map (fun d -> `String d) |> optional "description")
      @ security @ request @ parameters
      @ [ "responses", responses ~common endpoint ])

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

  let schemas endpoints =
    packed endpoints
    |> List.filter (fun (Packed e) -> not e.hidden)
    |> List.concat_map (fun (Packed e) ->
      (e.provide.request_body
      |> Option.map (fun ({ name; schema; _ } : Provide.body) -> name, schema)
      |> Option.to_list)
      @ List.map snd e.provide.rejections
      @ List.filter_map
          (fun (c : _ Response.case) ->
            match c.origin with
            | Produced { content = Schema (name, schema); _ } -> Some (name, schema)
            | Produced { content = No_content; _ } | Outside -> None)
          e.responses)
    |> List.fold_left add []

  (* OpenAPI forbids two operations of one method on a path, and two paths that differ only in the
     names of their parameters. The router would also only ever reach the first. *)
  let check_unique endpoints =
    let shape e = e.segments |> List.map (function Lit s -> s | Param _ -> "{}") in
    endpoints
    |> List.fold_left
         (fun seen (Packed e) ->
           seen
           |> List.iter (fun (Packed known) ->
             if shape known = shape e && known.template <> e.template then
               invalid_arg ("paths " ^ known.template ^ " and " ^ e.template ^ " are the same path");
             if known.template = e.template && known.meth = e.meth then
               invalid_arg (meth_to_string e.meth ^ " " ^ e.template ^ " is declared twice"));
           Packed e :: seen)
         []
    |> ignore

  let schemas_of = schemas

  let paths ?common endpoints =
    let visible = packed endpoints |> List.filter (fun (Packed e) -> not e.hidden) in
    check_unique visible;
    let ops =
      visible
      |> List.map @@ fun (Packed e) ->
         e.template, (String.lowercase_ascii (meth_to_string e.meth), operation ?common e)
    in
    ops
    |> List.fold_left (fun acc (t, _) -> if List.mem t acc then acc else t :: acc) []
    |> List.rev
    |> List.map @@ fun t ->
       t, `Assoc (ops |> List.filter_map @@ fun (t', op) -> if t = t' then Some op else None)

  let document ?servers ?tags ?security_schemes ?common ?(schemas = []) ~info endpoints =
    let optional key = Option.fold ~none:[] ~some:(fun v -> [ key, v ]) in
    let all = List.fold_left add (endpoints |> schemas_of) schemas in
    let components =
      Option.fold security_schemes ~none:[] ~some:(fun s -> [ "securitySchemes", `Assoc s ])
      @ [ "schemas", `Assoc (all |> List.map @@ fun (name, schema) -> pascal name, schema) ]
    in
    `Assoc
      ([ "openapi", `String "3.1.0"; "info", info ]
      @ optional "servers" (Option.map (fun s -> `List s) servers)
      @ optional "tags" (Option.map (fun t -> `List t) tags)
      @ [ "paths", `Assoc (paths ?common endpoints); "components", `Assoc components ])
end
