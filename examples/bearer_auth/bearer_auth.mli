(** Bearer tokens and the authenticated user.

    The token is part of the request: a field of the request record, sent by the client in the
    [Authorization] header. The user is not: the application derives it from the token by handling
    [Current], and ends the request when it cannot authenticate. A handler of an endpoint that
    declares [required] is therefore never called unauthenticated, and reads the user with
    [current]. *)

val bearer_token : string -> string option
(** The bearer token in the value of an [Authorization] header. Not verified. *)

val credential :
  scheme:string -> ('q -> string option) -> ('q, string option, 's, 's) Ocapi.Endpoint.Provide.t
(** The bearer token as presented. Nothing is verified, so nothing is required; [scheme] only goes
    into the documented security. *)

module Make (User : sig
  type t

  val scheme : string
  (** The name of the OpenAPI security scheme. *)
end) : sig
  type _ Effect.t += Current : User.t Effect.t

  val required : unit -> ('q, unit, 's, 's) Ocapi.Endpoint.Provide.t
  (** Authenticates before the handler runs. Documents [User.scheme] as the security of the
      endpoint, and the 401 the handler of [Current] answers when it cannot authenticate. *)

  val current : unit -> User.t
  (** The user, in the handler of an endpoint that declares [required]. *)

  val run : User.t -> (unit -> 'a) -> 'a
  (** Answers [Current] inside [f] with the given user, e.g. in tests. *)
end
