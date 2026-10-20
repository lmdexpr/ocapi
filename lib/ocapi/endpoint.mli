(** An HTTP endpoint as a value: method, path template, the values it takes from a request and
    the responses it can give. The server routing ([Server]), the client request
    ([Client]) and the OpenAPI document ({!Openapi}) are derived from that value.

    The only dependency is yojson; no HTTP library is involved. Schemas are plain
    [Yojson.Safe.t], so any generator can supply them.

    A request is a value of a type of the caller's, typically a record. Providers ({!Path},
    {!Query}, {!Body}, ...) say where each of its parts travels, combined with the applicative in
    {!Provide} and {!Syntax}: the server reads the record out of an HTTP request and hands it to
    the handler, the client writes the record it is given. *)

type meth = [ `GET | `POST | `PUT | `PATCH | `DELETE ]

val meth_to_string : meth -> string
(** Upper-case name, e.g. ["PATCH"]. *)

(** A value's name in the OpenAPI components, its schema, and its JSON encoder and decoder. *)
module Codec : sig
  type 'a t = {
    name : string;
    schema : Yojson.Safe.t;
    to_json : 'a -> Yojson.Safe.t;
    of_json : Yojson.Safe.t -> 'a;
  }
  (** [name] is the snake_case name of the schema in the components. The server encodes with
      [to_json], the client decodes with [of_json]; [of_json] raises on a value it rejects. *)

  val make :
    name:string -> schema:Yojson.Safe.t -> ('a -> Yojson.Safe.t) -> (Yojson.Safe.t -> 'a) -> 'a t
end

(** Where a value sits in a handler's result: [project] takes it out for the server, [inject] puts
    it back for the client. *)
module Prism : sig
  type ('r, 'a) t = { inject : 'a -> 'r; project : 'r -> 'a option }

  val make : inject:('a -> 'r) -> project:('r -> 'a option) -> ('r, 'a) t
  val id : ('a, 'a) t
  val ok : (('a, 'e) result, 'a) t
  val error : (('a, 'e) result, 'e) t
  val some : ('a option, 'a) t
  val none : ('a option, unit) t
end

(** What an endpoint reads from a request. A transport adapter builds this. *)
module Request : sig
  type t = {
    meth : meth;
    segments : string list;  (** Path split on ['/'], without empty segments. *)
    query : (string * string list) list;
    header : string -> string option;  (** Case-insensitive lookup. *)
    body : unit -> string;
      (** Reads the body when first called; the position of the body provider among the others
          decides when. An adapter reads it at most once and answers later calls with the same
          string. *)
    params : (string * string) list;
      (** Values of the [{name}] segments of the path template. The router fills this. *)
  }
end

(** What an endpoint answers. A transport adapter writes this. *)
module Reply : sig
  type body = Empty | Json of Yojson.Safe.t | Raw of string
  type t = { status : int; headers : (string * string) list; body : body }

  val json : ?headers:(string * string) list -> int -> Yojson.Safe.t -> t
  val empty : int -> t
end

(** What the client puts on the wire for a request: path parameters, query, headers and body. *)
module Encoded : sig
  type t = {
    params : (string * string) list;
    query : (string * string list) list;
    headers : (string * string) list;
    cookies : (string * string) list;  (** Sent together in one [Cookie] header. *)
    body : string option;
  }

  val empty : t
end

(** Both directions of one part of a request ['q]: the server reads an ['a] out of an HTTP
    request, the client writes its part of a ['q] into one. Each also documents what it reads. *)
module Provide : sig
  type body_unread
  type body_read

  type (-'q, +'a, +'i, +'o) t
  (** ['i] and ['o] are the state of the request body before and after the provider: only
      {!Body} providers turn {!body_unread} into {!body_read}, the others pass the state on. As
      {!both} chains the states and {!Endpoint.make} starts from {!body_unread}, an endpoint that
      would read its body twice does not compile. *)

  type parameter

  val parameter :
    in_:[ `Path | `Query | `Header | `Cookie ] ->
    required:bool ->
    ?example:Yojson.Safe.t ->
    description:string ->
    schema:Yojson.Safe.t ->
    string ->
    parameter

  val make :
    ?security:string list ->
    ?parameters:parameter list ->
    ?responses:(int * string) list ->
    ?encode:('q -> Encoded.t) ->
    (Request.t -> ('a, Reply.t) result) ->
    ('q, 'a, 's, 's) t
  (** A provider that does not read the body. [security] lists alternative security scheme
      names; the security schemes of several providers must all be satisfied. An [Error] reply ends the request; [responses] documents the
      statuses and descriptions of those replies. [encode] is the client's side; without it the
      client sends nothing for this provider (a value the server finds elsewhere, say). *)

  val return : 'a -> ('q, 'a, 's, 's) t
  val map : ('a -> 'b) -> ('q, 'a, 'i, 'o) t -> ('q, 'b, 'i, 'o) t

  val contramap : ('q2 -> 'q1) -> ('q1, 'a, 'i, 'o) t -> ('q2, 'a, 'i, 'o) t
  (** The provider for a part of a larger request: the client writes what [f] takes out of the
      request, the server reads as before. A provider shared by several endpoints is written once
      against its own type, e.g. [Provide.contramap (fun r -> r.page) paging]. *)

  val both : ('q, 'a, 'i, 'm) t -> ('q, 'b, 'm, 'o) t -> ('q, 'a * 'b, 'i, 'o) t
  (** Resolves left to right and answers with the reply of the first provider that fails. The
      client sends what both encode. *)

  val require :
    ?responses:(int * string) list ->
    ('e -> Reply.t) ->
    ('q, ('a, 'e) result, 'i, 'o) t ->
    ('q, 'a, 'i, 'o) t
  (** Turns a provider that may fail into one that only yields values, by choosing the reply
      for each failure. [responses] documents those replies. *)

  val require_json :
    status:int ->
    description:string ->
    'b Codec.t ->
    ('e -> 'b) ->
    ('q, ('a, 'e) result, 'i, 'o) t ->
    ('q, 'a, 'i, 'o) t
  (** {!require}, answering each failure with [status] and the codec's JSON of what [reject] makes
      of it. The document shows the codec's schema for [status], so it matches what is sent. *)

  val resolve : ('q, 'a, _, _) t -> Request.t -> ('a, Reply.t) result

  val encode : ('q, _, _, _) t -> 'q -> (Encoded.t, string) result
  (** [Error] with the reason for a path or query value the server's {!Value} would not parse
      back. *)
end

module Syntax : sig
  val ( let+ ) : ('q, 'a, 'i, 'o) Provide.t -> ('a -> 'b) -> ('q, 'b, 'i, 'o) Provide.t

  val ( and+ ) :
    ('q, 'a, 'i, 'm) Provide.t -> ('q, 'b, 'm, 'o) Provide.t -> ('q, 'a * 'b, 'i, 'o) Provide.t
end

(** A path or query value: its parser, its printer and its schema. *)
module Value : sig
  type 'a t

  val make : schema:Yojson.Safe.t -> (string -> 'a option) -> ('a -> string) -> 'a t
  (** A smart constructor fits as the parser. *)

  val string : string t

  val int : ?minimum:int -> ?maximum:int -> unit -> int t
  (** Decimal digits with an optional [-]. Checks the range and documents it with [minimum] /
      [maximum] from the same arguments. *)

  val int_schema : ?minimum:int -> ?maximum:int -> unit -> Yojson.Safe.t

  val bool : bool t
  (** [true] or [false]. *)
end

(** The providers of path, query and body take a getter: the field of the request record they
    stand for, e.g. [Path.string "id" (fun r -> r.id)]. *)

module Path : sig
  val valid : string -> bool
  (** Whether a value is accepted as a path parameter: a non-empty string of letters, digits,
      ['_'] and ['-']. No route matches a segment with anything else ([..], percent-encoded
      characters, ...). *)

  val value :
    ?description:string -> 'a Value.t -> string -> ('q -> 'a) -> ('q, 'a, 's, 's) Provide.t
  (** The value of the [{name}] segment of the path template. A segment that does not parse is a
      404. *)

  val string : ?description:string -> string -> ('q -> string) -> ('q, string, 's, 's) Provide.t
end

module Query : sig
  type error = [ `Missing | `Invalid ]

  val required :
    ?unique:bool ->
    ?example:Yojson.Safe.t ->
    description:string ->
    'a Value.t ->
    string ->
    ('q -> 'a) ->
    ('q, ('a, error) result, 's, 's) Provide.t
  (** The value of the first occurrence of the key; several values are joined with [','] before
      parsing, as [Uri.get_query_param] does. With [unique], a key that occurs more than once is
      [`Invalid]. *)

  val optional :
    ?unique:bool ->
    ?example:Yojson.Safe.t ->
    description:string ->
    'a Value.t ->
    string ->
    ('q -> 'a option) ->
    ('q, ('a option, error) result, 's, 's) Provide.t
  (** [Ok None] without the key; [`Invalid] when the key is there but its value does not parse.
      The client leaves the key out for [None]. *)
end

module Header : sig
  type error = Query.error

  val required :
    ?example:Yojson.Safe.t ->
    description:string ->
    'a Value.t ->
    string ->
    ('q -> 'a) ->
    ('q, ('a, error) result, 's, 's) Provide.t
  (** The value of the header, looked up case-insensitively. [Authorization], [Content-Type] and
      [Accept] are not parameters in OpenAPI ([Invalid_argument]): declare the first with
      [Provide.make ~security], the others come with the media types. *)

  val optional :
    ?example:Yojson.Safe.t ->
    description:string ->
    'a Value.t ->
    string ->
    ('q -> 'a option) ->
    ('q, ('a option, error) result, 's, 's) Provide.t
  (** [Ok None] without the header. The client leaves it out for [None]. *)
end

module Cookie : sig
  type error = Query.error

  val required :
    ?example:Yojson.Safe.t ->
    description:string ->
    'a Value.t ->
    string ->
    ('q -> 'a) ->
    ('q, ('a, error) result, 's, 's) Provide.t
  (** The value of the cookie [name] in the [Cookie] header. The client refuses to send a value
      with characters a cookie cannot hold (spaces, [;], [,], quotes, ...). *)

  val optional :
    ?example:Yojson.Safe.t ->
    description:string ->
    'a Value.t ->
    string ->
    ('q -> 'a option) ->
    ('q, ('a option, error) result, 's, 's) Provide.t
  (** [Ok None] without the cookie. The client leaves it out for [None]. *)
end

module Body : sig
  val json :
    'a Codec.t ->
    ('q -> 'a) ->
    ('q, ('a, [ `Malformed of string ]) result, Provide.body_unread, Provide.body_read) Provide.t
  (** The body as JSON, decoded with the codec; an exception from its [of_json] is [`Malformed]
      too. The string is the exception's text, meant for logs rather than for the client. Failures
      of reading the body itself propagate. The document shows the codec's schema. *)

  val raw :
    name:string ->
    schema:Yojson.Safe.t ->
    content_type:string ->
    ('q -> string) ->
    ('q, string, Provide.body_unread, Provide.body_read) Provide.t
  (** Passes the body through. The client sends it with [content_type], and the document shows the
      schema under that media type. *)
end

(** How a handler's result becomes a response, and a response the result again. Cases are tried in
    declaration order. *)
module Response : sig
  type 'r case

  val empty : status:int -> description:string -> ('r, unit) Prism.t -> 'r case
  val json : status:int -> description:string -> 'a Codec.t -> ('r, 'a) Prism.t -> 'r case

  val json_with_headers :
    status:int ->
    description:string ->
    ?headers:(string * string) list ->
    'a Codec.t ->
    ('r, (string * string) list * 'a) Prism.t ->
    'r case
  (** {!json} with headers chosen per response ([Set-Cookie], say): the server sends the headers
      taken out of the result with the value, the client gets back every header it received.
      [headers] documents header names with their descriptions. *)

  val unspecified_json : status:int -> description:string -> ('r, Yojson.Safe.t) Prism.t -> 'r case
  (** Sends JSON without documenting a schema. *)

  val raw :
    status:int -> description:string -> ('r, (string * string) list * string) Prism.t -> 'r case
  (** The handler decides the headers and the body (a download, say). No content is documented. *)

  val outside : status:int -> description:string -> 'r case
  (** Documents a response that does not go through the endpoint (one from the router, the
      adapter or the application's catch-all); a provider declares its own replies in
      [responses]. The client cannot decode it. A status declared only by such cases is
      documented without the endpoint headers. *)

  val errors :
    ?without_body:int list ->
    'e Codec.t ->
    status:('e -> int) ->
    ('r, 'e) Prism.t ->
    (int * string) list ->
    'r case list
  (** Error cases from a list of documented statuses and descriptions. An error taken out of the
      result matches the case of the status [status] gives it; an error mapped to an undocumented
      status matches no case. Statuses in [without_body] send no body, so the client cannot decode
      them. *)
end

type ('q, 'r) t
(** ['q] is the request, a value the server reads and the client writes; ['r] is what the handler
    returns, a value the server encodes and the client decodes. *)

type header = { name : string; value : string; description : string option }
(** A header sent with every response of an endpoint. Only those with a [description] are
    documented. *)

val make :
  meth:meth ->
  string ->
  ?hidden:bool ->
  ?headers:header list ->
  tags:string list ->
  summary:string ->
  ?description:string ->
  ('q, 'q, Provide.body_unread, _) Provide.t ->
  responses:'r Response.case list ->
  ('q, 'r) t
(** The second argument is the path template, e.g. [/items/{id}/tags]. Its [{name}]
    segments must be exactly the path parameters the provider declares and [responses] must not be
    empty, else [Invalid_argument].
    [responses] are matched in declaration order and documented in status order. A [hidden]
    endpoint is left out of the OpenAPI document. *)

val meth : _ t -> meth
val template : _ t -> string

val params : _ t -> string list -> (string * string) list option
(** The path parameters when the segments of a request path match the template: literals must be
    equal, a [{name}] segment takes any {!Path.valid} value. *)

val resolve : ('q, _) t -> Request.t -> ('q, Reply.t) result
(** Runs the providers on a request whose [params] are filled. A failing provider's reply carries
    the endpoint headers. *)

val encode : ('q, _) t -> 'q -> (Encoded.t, string) result
(** What the client sends for a request; [Error] as {!Provide.encode}. *)

val reply : (_, 'r) t -> 'r -> Reply.t
(** The response of the first case that matches a handler's result, with the endpoint headers.
    [Failure] when no case matches. *)

val decode :
  (_, 'r) t ->
  status:int ->
  headers:(string * string) list ->
  string ->
  ('r, [ `Undeclared | `Malformed of string ]) result
(** The result a response stands for: the first case of its status that decodes it. [`Undeclared]
    when no case of the status can, [`Malformed] with the reason when the body does not decode. *)

(** The OpenAPI 3 document pieces an endpoint contributes. *)
module Openapi : sig
  type ('a, 'r) endpoint := ('a, 'r) t

  (** Endpoints of any types, written as a list literal: [Openapi.document ~info [ get; post ]]
      where an {!Endpoints.t} is expected, or [Openapi.Endpoints.[ get; post ]] anywhere else.
      Without either, the literal is an ordinary list and endpoints of different types do not fit
      in it. *)
  module Endpoints : sig
    type t = [] : t | ( :: ) : (_, _) endpoint * t -> t

    val append : t -> t -> t
  end

  val component_name : string -> string
  (** The components name (PascalCase) of a schema name (snake_case). *)

  type common = {
    status : int;
    description : string;
    scope : [ `Every_operation | `Operations_with_body ];
  }
  (** A response given outside the endpoints, by the transport for instance, documented on every
      operation or on those with a request body. *)

  val operation : ?common:common list -> (_, _) t -> Yojson.Safe.t
  (** The responses of one status are documented from the endpoint's cases first, then from what
      its providers declare, then from [common]. *)

  val schemas : Endpoints.t -> (string * Yojson.Safe.t) list
  (** The schemas the endpoints refer to in their request bodies and responses, in order of
      appearance, without duplicates. Hidden endpoints are left out. [Invalid_argument] when one
      name is declared with two different schemas. *)

  val paths : ?common:common list -> Endpoints.t -> (string * Yojson.Safe.t) list
  (** One path item per path template; endpoints with the same template share an item. Hidden
      endpoints are left out. *)

  val document :
    ?servers:Yojson.Safe.t list ->
    ?tags:Yojson.Safe.t list ->
    ?security_schemes:(string * Yojson.Safe.t) list ->
    ?common:common list ->
    ?schemas:(string * Yojson.Safe.t) list ->
    info:Yojson.Safe.t ->
    Endpoints.t ->
    Yojson.Safe.t
  (** An OpenAPI 3.1 document: [info], {!paths} and, under [components], {!val-schemas} and the
      [schemas] given (snake_case names, shared definitions the others refer to, say) by their
      {!component_name}, and the [security_schemes] as given. *)
end
