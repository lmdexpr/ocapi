(* The attributes the derivers read, and the check that no misspelled one goes unread.

   The compiler ignores an attribute nobody reads, so [[@heder "X-Trace"]] would quietly make a
   query parameter. On what the derivers read, an attribute close to one of theirs, or in their
   namespace, or meant for the other kind of declaration, is an error. Others (another ppx's) are
   left alone. *)

open Ppxlib

(* Every attribute is also accepted with the [ocapi.] prefix, e.g. [[@ocapi.path]]. *)
let name_payload = Ast_pattern.(alt_option (single_expr_payload (estring __)) (pstr nil))
let expr_payload = Ast_pattern.(single_expr_payload __)

let field ?(k = Fun.id) name payload =
  Attribute.declare ("ocapi." ^ name) Attribute.Context.label_declaration payload k

let constructor name payload =
  Attribute.declare ("ocapi." ^ name) Attribute.Context.constructor_declaration payload Fun.id

let path = field "path" name_payload
let query = field "query" name_payload
let header = field "header" name_payload
let cookie = field "cookie" name_payload

let body =
  Attribute.declare "ocapi.body" Attribute.Context.label_declaration Ast_pattern.(pstr nil) ()

let provide = field "provide" expr_payload
let value = field "value" expr_payload
let field_codec = field "codec" expr_payload
let default = field "default" expr_payload
let reject = field "reject" expr_payload
let status = constructor "status" Ast_pattern.(single_expr_payload (eint __))
let constructor_codec = constructor "codec" expr_payload

let discriminator =
  Attribute.declare "ocapi.discriminator" Attribute.Context.constructor_declaration
    Ast_pattern.(single_expr_payload (pexp_tuple (estring __ ^:: estring __ ^:: nil)))
    (fun property value -> property, value)

let field_names =
  [ "path"; "query"; "header"; "cookie"; "body"; "provide"; "value"; "codec"; "default"; "reject" ]

let constructor_names = [ "status"; "codec"; "discriminator" ]

let distance a b =
  let la = String.length a and lb = String.length b in
  let row = Array.init (lb + 1) Fun.id in
  for i = 1 to la do
    let previous = ref row.(0) in
    row.(0) <- i;
    for j = 1 to lb do
      let current = row.(j) in
      row.(j) <-
        min
          (min (row.(j) + 1) (row.(j - 1) + 1))
          (!previous + if a.[i - 1] = b.[j - 1] then 0 else 1);
      previous := current
    done
  done;
  row.(lb)

let check ~known ~others ~what attributes =
  let elsewhere = List.filter (fun a -> not (List.mem a known)) others in
  attributes
  |> List.iter @@ fun attribute ->
     let loc = attribute.attr_name.loc in
     let full = attribute.attr_name.txt in
     let prefixed = String.starts_with ~prefix:"ocapi." full in
     let name = if prefixed then String.sub full 6 (String.length full - 6) else full in
     if List.mem name known then
       ()
     else if List.mem name elsewhere then
       Location.raise_errorf ~loc "ocapi: [@%s] does not apply to a %s" full what
     else if prefixed then
       Location.raise_errorf ~loc "ocapi: unknown attribute [@%s]" full
     else if not (String.contains name '.') then
       match
         List.find_opt (fun k -> distance name k <= 2) known
       with
       | Some k ->
         Location.raise_errorf ~loc "ocapi: unknown attribute [@%s]; did you mean [@%s]?" full k
       | None -> ()

let check_field (ld : label_declaration) =
  check ~known:field_names ~others:constructor_names ~what:"field" ld.pld_attributes

let check_constructor (cd : constructor_declaration) =
  check ~known:constructor_names ~others:field_names ~what:"constructor" cd.pcd_attributes
