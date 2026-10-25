(** Schemas derived by ppx_deriving_jsonschema in {!Ocapi.Endpoint} declarations.

    A derived schema refers to its own definitions with [#/$defs/...], relative to the schema, and
    marks embedded schemas with a [file://] [$id] naming the source file. Placed under the
    components of an OpenAPI document, those references would resolve against the document root.
    These functions rewrite them into references from the document root and drop the [file://]
    markers, so recursive types and the types that contain them document as they are. A
    [#/$defs/x] the schema does not define is a shared definition: a component of its own, see
    {!components}. {!documents} turns the schemas back into standalone files. *)

type schema = Ppx_deriving_jsonschema_runtime.t

val schema : name:string -> schema -> Yojson.Safe.t
(** The schema as it sits under the components as [name] (snake_case). *)

val codec :
  name:string ->
  schema ->
  ('a -> Yojson.Safe.t) ->
  (Yojson.Safe.t -> 'a) ->
  ('a, 'd) Ocapi.Endpoint.Codec.t

val components : (string * schema) list -> (string * Yojson.Safe.t) list
(** Shared definitions (snake_case names) as components, for [Ocapi_openapi.document
    ~schemas]. A [#/$defs/x] that a schema does not define itself refers to the component [x]. *)

val documents :
  ?definitions:(string * schema) list -> Ocapi_openapi.Endpoints.t -> (string * Yojson.Safe.t) list
(** The schemas the endpoints refer to (snake_case names, in order of appearance), each as a
    standalone JSON Schema document with a title. References inside the schema are taken from the
    document root again; references to [definitions] go back to [#/$defs], where the definitions
    are added. *)
