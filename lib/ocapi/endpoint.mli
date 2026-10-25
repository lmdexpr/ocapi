(** An HTTP endpoint as a value: method, path template, the values it takes from a request and
    the responses it can give. The server routing ([Server]), the client request ([Client]) and,
    through {!Meta}, the OpenAPI document ([Ocapi_openapi]) are derived from that value.

    The only dependency is yojson; no HTTP library is involved. Schemas are plain
    [Yojson.Safe.t], so any generator can supply them.

    A request is a value of a type of the caller's, typically a record. Providers ({!Path},
    {!Query}, {!Body}, ...) say where each of its parts travels, combined with the applicative in
    {!Provide} and {!Syntax}: the server reads the record out of an HTTP request and hands it to
    the handler, the client writes the record it is given. *)

type meth = [ `GET | `POST | `PUT | `PATCH | `DELETE ]

val meth_to_string : meth -> string
(** Upper-case name, e.g. ["PATCH"]. *)

(** {1 Whether an endpoint can be documented}

    The last type parameter of codecs, values, providers, response cases and endpoints says
    whether every part of it has a schema. A part built with a schema leaves the parameter
    polymorphic; a part without one ({!Codec.custom}, {!Value.custom},
    {!Response.unspecified_json}) fixes it to {!schemaless}, and so does everything it is combined
    into. The OpenAPI document ([Ocapi_openapi]) takes only {!complete} endpoints. Nothing needs
    annotating: the parameter is inferred, and a type error points at the part without a
    schema. *)

type schemaless
(** Some part has no schema. *)

type complete
(** Every part has a schema. *)

(** A value's schema, its JSON encoder and decoder, and optionally its name in the OpenAPI
    components. *)
module Codec : sig
  type ('a, +'d) t

  val make :
    ?name:string ->
    schema:Yojson.Safe.t ->
    ('a -> Yojson.Safe.t) ->
    (Yojson.Safe.t -> 'a) ->
    ('a, 'd) t
  (** [name] is the snake_case name of the schema in the components; without it, the schema is
      written inline where it is used. The server encodes with [to_json], the client decodes with
      [of_json]; [of_json] raises on a value it rejects. *)

  val custom : ('a -> Yojson.Safe.t) -> (Yojson.Safe.t -> 'a) -> ('a, schemaless) t
  (** A codec without a schema: it travels, but cannot be documented. *)

  val name : (_, _) t -> string option
  (** The snake_case components name, if the codec was given one. *)

  val schema : (_, complete) t -> Yojson.Safe.t
  (** The schema, of a codec that has one: a {!custom} codec has none to give. *)

  val to_json : ('a, _) t -> 'a -> Yojson.Safe.t
  (** What the server sends. *)

  val of_json : ('a, _) t -> Yojson.Safe.t -> 'a
  (** What the client reads back; raises on a value the codec rejects. *)
end

(** Where a value sits in a handler's result: [project] takes it out for the server, [inject] puts
    it back for the client. [constructor] names the place for the document ([x-ocapi-cases]), so
    that a generator can rebuild the result type: [ok] and [error] are [Ok] and [Error]. *)
module Prism : sig
  type ('r, 'a) t = { inject : 'a -> 'r; project : 'r -> 'a option; constructor : string option }

  val make : inject:('a -> 'r) -> project:('r -> 'a option) -> ('r, 'a) t
  (** Without a constructor name; see {!named}. *)

  val named : string -> ('r, 'a) t -> ('r, 'a) t
  (** The prism with a constructor name, e.g. [Prism.make ~inject ~project |> Prism.named "Gone"]. *)

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

  type (-'q, +'a, +'i, +'o, +'d) t
  (** ['i] and ['o] are the state of the request body before and after the provider: only
      {!Body} providers turn {!body_unread} into {!body_read}, the others pass the state on. As
      {!both} chains the states and {!Endpoint.make} starts from {!body_unread}, an endpoint that
      would read its body twice does not compile. *)

  type parameter = private {
    name : string;
    in_ : [ `Path | `Query | `Header | `Cookie ];
    required : bool;
    description : string option;
    schema : Yojson.Safe.t;
    example : Yojson.Safe.t option;
  }
  (** What a provider documents of a path, query, header or cookie value; built with
      {!val-parameter}. *)

  val parameter :
    in_:[ `Path | `Query | `Header | `Cookie ] ->
    required:bool ->
    ?example:Yojson.Safe.t ->
    ?description:string ->
    schema:Yojson.Safe.t ->
    string ->
    parameter

  val make :
    ?security:string list ->
    ?parameters:parameter list ->
    ?responses:(int * string) list ->
    ?encode:('q -> Encoded.t) ->
    (Request.t -> ('a, Reply.t) result) ->
    ('q, 'a, 's, 's, 'd) t
  (** A provider that does not read the body. [security] lists alternative security scheme
      names; the security schemes of several providers must all be satisfied. An [Error] reply ends the request; [responses] documents the
      statuses and descriptions of those replies. [encode] is the client's side; without it the
      client sends nothing for this provider (a value the server finds elsewhere, say). *)

  val return : 'a -> ('q, 'a, 's, 's, 'd) t
  val map : ('a -> 'b) -> ('q, 'a, 'i, 'o, 'd) t -> ('q, 'b, 'i, 'o, 'd) t

  val contramap : ('q2 -> 'q1) -> ('q1, 'a, 'i, 'o, 'd) t -> ('q2, 'a, 'i, 'o, 'd) t
  (** The provider for a part of a larger request: the client writes what [f] takes out of the
      request, the server reads as before. A provider shared by several endpoints is written once
      against its own type, e.g. [Provide.contramap (fun r -> r.page) paging]. *)

  val both : ('q, 'a, 'i, 'm, 'd) t -> ('q, 'b, 'm, 'o, 'd) t -> ('q, 'a * 'b, 'i, 'o, 'd) t
  (** Resolves left to right and answers with the reply of the first provider that fails. The
      client sends what both encode. *)

  val require :
    ?responses:(int * string) list ->
    ('e -> Reply.t) ->
    ('q, ('a, 'e) result, 'i, 'o, 'd) t ->
    ('q, 'a, 'i, 'o, 'd) t
  (** Turns a provider that may fail into one that only yields values, by choosing the reply
      for each failure. [responses] documents those replies. *)

  val require_json :
    status:int ->
    ?description:string ->
    ('b, 'd) Codec.t ->
    ('e -> 'b) ->
    ('q, ('a, 'e) result, 'i, 'o, 'd) t ->
    ('q, 'a, 'i, 'o, 'd) t
  (** {!require}, answering each failure with [status] and the codec's JSON of what [reject] makes
      of it. The document shows the codec's schema for [status], so it matches what is sent. *)

  val resolve : ('q, 'a, _, _, _) t -> Request.t -> ('a, Reply.t) result

  val encode : ('q, _, _, _, _) t -> 'q -> (Encoded.t, string) result
  (** [Error] with the reason for a path or query value the server's {!Value} would not parse
      back. *)
end

module Syntax : sig
  val ( let+ ) : ('q, 'a, 'i, 'o, 'd) Provide.t -> ('a -> 'b) -> ('q, 'b, 'i, 'o, 'd) Provide.t

  val ( and+ ) :
    ('q, 'a, 'i, 'm, 'd) Provide.t ->
    ('q, 'b, 'm, 'o, 'd) Provide.t ->
    ('q, 'a * 'b, 'i, 'o, 'd) Provide.t
end

(** A path or query value: its parser, its printer and its schema. *)
module Value : sig
  type ('a, +'d) t

  val make : schema:Yojson.Safe.t -> (string -> 'a option) -> ('a -> string) -> ('a, 'd) t
  (** A smart constructor fits as the parser. *)

  val custom : (string -> 'a option) -> ('a -> string) -> ('a, schemaless) t
  (** A value without a schema: it travels, but cannot be documented. *)

  val string : (string, 'd) t

  val int : ?minimum:int -> ?maximum:int -> unit -> (int, 'd) t
  (** Decimal digits with an optional [-]. Checks the range and documents it with [minimum] /
      [maximum] from the same arguments. *)

  val int_schema : ?minimum:int -> ?maximum:int -> unit -> Yojson.Safe.t

  val bool : (bool, 'd) t
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
    ?description:string ->
    ('a, 'd) Value.t ->
    string ->
    ('q -> 'a) ->
    ('q, 'a, 's, 's, 'd) Provide.t
  (** The value of the [{name}] segment of the path template. A segment that does not parse is a
      404. *)

  val string : ?description:string -> string -> ('q -> string) -> ('q, string, 's, 's, 'd) Provide.t
end

module Query : sig
  type error = [ `Missing | `Invalid ]

  val required :
    ?unique:bool ->
    ?example:Yojson.Safe.t ->
    ?description:string ->
    ('a, 'd) Value.t ->
    string ->
    ('q -> 'a) ->
    ('q, ('a, error) result, 's, 's, 'd) Provide.t
  (** The value of the first occurrence of the key; several values are joined with [','] before
      parsing, as [Uri.get_query_param] does. With [unique], a key that occurs more than once is
      [`Invalid]. *)

  val optional :
    ?unique:bool ->
    ?example:Yojson.Safe.t ->
    ?description:string ->
    ('a, 'd) Value.t ->
    string ->
    ('q -> 'a option) ->
    ('q, ('a option, error) result, 's, 's, 'd) Provide.t
  (** [Ok None] without the key; [`Invalid] when the key is there but its value does not parse.
      The client leaves the key out for [None]. *)
end

module Header : sig
  type error = Query.error

  val required :
    ?example:Yojson.Safe.t ->
    ?description:string ->
    ('a, 'd) Value.t ->
    string ->
    ('q -> 'a) ->
    ('q, ('a, error) result, 's, 's, 'd) Provide.t
  (** The value of the header, looked up case-insensitively. [Authorization], [Content-Type] and
      [Accept] are not parameters in OpenAPI ([Invalid_argument]): declare the first with
      [Provide.make ~security], the others come with the media types. *)

  val optional :
    ?example:Yojson.Safe.t ->
    ?description:string ->
    ('a, 'd) Value.t ->
    string ->
    ('q -> 'a option) ->
    ('q, ('a option, error) result, 's, 's, 'd) Provide.t
  (** [Ok None] without the header. The client leaves it out for [None]. *)
end

module Cookie : sig
  type error = Query.error

  val required :
    ?example:Yojson.Safe.t ->
    ?description:string ->
    ('a, 'd) Value.t ->
    string ->
    ('q -> 'a) ->
    ('q, ('a, error) result, 's, 's, 'd) Provide.t
  (** The value of the cookie [name] in the [Cookie] header. The client refuses to send a value
      with characters a cookie cannot hold (spaces, [;], [,], quotes, ...). *)

  val optional :
    ?example:Yojson.Safe.t ->
    ?description:string ->
    ('a, 'd) Value.t ->
    string ->
    ('q -> 'a option) ->
    ('q, ('a option, error) result, 's, 's, 'd) Provide.t
  (** [Ok None] without the cookie. The client leaves it out for [None]. *)
end

module Body : sig
  val json :
    ('a, 'd) Codec.t ->
    ('q -> 'a) ->
    ( 'q,
      ('a, [ `Malformed of string ]) result,
      Provide.body_unread,
      Provide.body_read,
      'd )
    Provide.t
  (** The body as JSON, decoded with the codec; an exception from its [of_json] is [`Malformed]
      too. The string is the exception's text, meant for logs rather than for the client. Failures
      of reading the body itself propagate. The document shows the codec's schema. *)

  val raw :
    ?name:string ->
    schema:Yojson.Safe.t ->
    content_type:string ->
    ('q -> string) ->
    ('q, string, Provide.body_unread, Provide.body_read, 'd) Provide.t
  (** Passes the body through. The client sends it with [content_type], and the document shows the
      schema under that media type. *)
end

(** How a handler's result becomes a response, and a response the result again. Cases are tried in
    declaration order. A case without a [description] is documented with the reason phrase of its
    status ([OK], [Not Found], ...), as OpenAPI requires one. *)
module Response : sig
  type ('r, +'d) case

  val empty : status:int -> ?description:string -> ('r, unit) Prism.t -> ('r, 'd) case

  val json :
    status:int ->
    ?description:string ->
    ?discriminator:string * string ->
    ('a, 'd) Codec.t ->
    ('r, 'a) Prism.t ->
    ('r, 'd) case
  (** [discriminator] is a property of the codec's JSON and its value in this case, e.g.
      [("kind", "item")]. When several cases share a status, the document writes their schemas as
      a [oneOf] with a [discriminator] mapping each value to its schema: either every case of the
      status with a body has one, with the same property and a named codec, or none has, else
      {!Endpoint.make} raises [Invalid_argument]. Nothing checks that the JSON carries the value;
      the codec does. *)

  val json_with_headers :
    status:int ->
    ?description:string ->
    ?headers:(string * string) list ->
    ?discriminator:string * string ->
    ('a, 'd) Codec.t ->
    ('r, (string * string) list * 'a) Prism.t ->
    ('r, 'd) case
  (** {!json} with headers chosen per response ([Set-Cookie], say): the server sends the headers
      taken out of the result with the value, the client gets back every header it received.
      [headers] documents header names with their descriptions. *)

  val unspecified_json :
    status:int -> ?description:string -> ('r, Yojson.Safe.t) Prism.t -> ('r, schemaless) case
  (** Sends JSON without documenting a schema. *)

  val raw :
    status:int ->
    ?description:string ->
    ('r, (string * string) list * string) Prism.t ->
    ('r, 'd) case
  (** The handler decides the headers and the body (a download, say). No content is documented. *)

  val outside : ?description:string -> status:int -> unit -> ('r, 'd) case
  (** Documents a response that does not go through the endpoint (one from the router, the
      adapter or the application's catch-all); a provider declares its own replies in
      [responses]. The client cannot decode it. A status declared only by such cases is
      documented without the endpoint headers. *)

  val errors :
    ?without_body:int list ->
    ?discriminator:string * string ->
    ('e, 'd) Codec.t ->
    status:('e -> int) ->
    ('r, 'e) Prism.t ->
    (int * string) list ->
    ('r, 'd) case list
  (** Error cases from a list of documented statuses and descriptions. An error taken out of the
      result matches the case of the status [status] gives it; an error mapped to an undocumented
      status matches no case. Statuses in [without_body] send no body, so the client cannot decode
      them. [discriminator] is as in {!json}, the same for every case with a body. *)
end

type ('q, 'r, +'d) t
(** ['q] is the request, a value the server reads and the client writes; ['r] is what the handler
    returns, a value the server encodes and the client decodes; ['d] is {!complete} or
    {!schemaless}. *)

type header = { name : string; value : string; description : string option }
(** A header sent with every response of an endpoint. Only those with a [description] are
    documented. *)

val make :
  meth:meth ->
  string ->
  ?hidden:bool ->
  ?headers:header list ->
  ?tags:string list ->
  ?summary:string ->
  ?description:string ->
  ?operation_id:string ->
  ('q, 'q, Provide.body_unread, _, 'd) Provide.t ->
  responses:('r, 'd) Response.case list ->
  ('q, 'r, 'd) t
(** The second argument is the path template, e.g. [/items/{id}/tags]. Its [{name}]
    segments must be exactly the path parameters the provider declares, [responses] must not be
    empty and the discriminators of each status must agree ({!Response.json}), else
    [Invalid_argument].
    [responses] are matched in declaration order and documented in status order. A [hidden]
    endpoint is left out of the OpenAPI document. [tags], [summary], [description] and
    [operation_id] (the name generators give the call, unique in a document) are documented when
    given. *)

(** {1 One endpoint, one module}

    An endpoint declared as a module: its request and result types next to how they travel.
    [Make] checks the declaration as {!make} does, when the module is defined:

    {[
      module Get = struct
        module Spec = struct
          type request = { id : string }
          type nonrec result = (item, string) result
          type body = Endpoint.Provide.body_unread
          type doc = Endpoint.complete

          include Endpoint.Defaults

          let meth = `GET
          let path = "/items/{id}"
          let provide = ...
          let responses = ...
        end

        include Spec
        include Endpoint.Make (Spec)
      end
    ]}

    [Spec] is named because a functor applied to an anonymous structure cannot return the types
    defined in it. The resulting module is an {!S}: [Server.route (module Get)] and
    [Client.invoke (module Get)] take it whole. *)

(** What {!Make} takes. [body] is the state of the request body after [provide]
    ({!Provide.body_unread} when there is none); [doc] is {!complete} for an endpoint to document. *)
module type SPEC = sig
  type request
  type result
  type body
  type doc

  val meth : meth
  val path : string
  val hidden : bool
  val headers : header list
  val tags : string list
  val summary : string option
  val description : string option
  val operation_id : string option
  val provide : (request, request, Provide.body_unread, body, doc) Provide.t
  val responses : (result, doc) Response.case list
end

(** The optional parts of a {!SPEC}, to [include] and override: not hidden, no headers, no tags,
    no summary, description or operation id. *)
module Defaults : sig
  val hidden : bool
  val headers : header list
  val tags : string list
  val summary : string option
  val description : string option
  val operation_id : string option
end

(** An endpoint with its request and result types. *)
module type S = sig
  type request
  type result
  type doc

  val endpoint : (request, result, doc) t
end

module Make (Spec : SPEC) : sig
  val endpoint : (Spec.request, Spec.result, Spec.doc) t
  (** [Invalid_argument] as {!make}, when the functor is applied. *)
end

val meth : (_, _, _) t -> meth
val template : (_, _, _) t -> string

val params : (_, _, _) t -> string list -> (string * string) list option
(** The path parameters when the segments of a request path match the template: literals must be
    equal, a [{name}] segment takes any {!Path.valid} value. *)

val resolve : ('q, _, _) t -> Request.t -> ('q, Reply.t) result
(** Runs the providers on a request whose [params] are filled. A failing provider's reply carries
    the endpoint headers. *)

val encode : ('q, _, _) t -> 'q -> (Encoded.t, string) result
(** What the client sends for a request; [Error] as {!Provide.encode}. *)

val reply : (_, 'r, _) t -> 'r -> Reply.t
(** The response of the first case that matches a handler's result, with the endpoint headers.
    [Failure] when no case matches. *)

val decode :
  (_, 'r, _) t ->
  status:int ->
  headers:(string * string) list ->
  string ->
  ('r, [ `Undeclared | `Malformed of string ]) result
(** The result a response stands for: the first case of its status that decodes it. [`Undeclared]
    when no case of the status can, [`Malformed] with the reason when the body does not decode. *)

type common = {
  status : int;
  description : string;
  scope : [ `Every_operation | `Operations_with_body ];
}
(** A response given outside the endpoints, by the transport for instance, to document on every
    operation or on those with a request body. *)

(** What a {!complete} endpoint declares, as plain data: what a document generator such as
    [Ocapi_openapi] reads. Nothing here sends or receives. *)
module Meta : sig
  type schema = { name : string option; schema : Yojson.Safe.t }
  (** A named schema belongs in the components; an unnamed one is written where it is used. *)

  type parameter = Provide.parameter = private {
    name : string;
    in_ : [ `Path | `Query | `Header | `Cookie ];
    required : bool;
    description : string option;
    schema : Yojson.Safe.t;
    example : Yojson.Safe.t option;
  }

  type body = { schema : schema; content_type : string }

  type case = {
    status : int;
    description : string option;
    content : schema option;  (** The JSON body's schema; [None] for no documented body. *)
    headers : (string * string) list;  (** Names and descriptions of headers chosen per response. *)
    outside : bool;  (** Declared by {!Response.outside}: not sent through the endpoint. *)
    constructor : string option;  (** From the prism: {!Prism.named}. *)
    discriminator : (string * string) option;
  }

  type endpoint = {
    meth : meth;
    template : string;
    segments : [ `Lit of string | `Param of string ] list;
    hidden : bool;
    headers : header list;
    tags : string list;
    summary : string option;
    operation_id : string option;
    description : string option;
    security : string list list;
      (** One list of alternative scheme names per provider; every provider must be satisfied. *)
    parameters : parameter list;  (** In provider order. *)
    request_body : body option;
    responses : (int * string option) list;
      (** The replies the providers declare ({!Provide.require} and the like). *)
    rejections : (int * schema) list;  (** The bodies of those replies, by status. *)
    cases : case list;  (** In declaration order. *)
  }

  val of_endpoint : (_, _, complete) t -> endpoint
  (** Only {!complete} endpoints: a part without a schema would read as the empty schema, which
      accepts anything, and a body sent with {!Response.unspecified_json} as no body at all. *)
end
