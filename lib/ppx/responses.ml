(* [[@@deriving responses]]: a result variant sent as responses, one per constructor. *)

open Ppxlib

let case ~loc ~many (cd : constructor_declaration) =
  Attributes.check_constructor cd;
  let name = cd.pcd_name.txt in
  let constr arg = Ast_builder.Default.pexp_construct ~loc { txt = Lident name; loc } arg in
  let pconstr arg = Ast_builder.Default.ppat_construct ~loc { txt = Lident name; loc } arg in
  let others =
    if many then [ Ast_builder.Default.case ~lhs:[%pat? _] ~guard:None ~rhs:[%expr None] ] else []
  in
  let status =
    match Attribute.get Attributes.status cd with
    | Some status -> Ast_builder.Default.eint ~loc status
    | None -> Location.raise_errorf ~loc:cd.pcd_loc "ocapi: %s has no [@status]" name
  in
  let description = Doc.description ~loc cd.pcd_attributes in
  let named prism =
    [%expr Ocapi.Endpoint.Prism.named [%e Ast_builder.Default.estring ~loc name] [%e prism]]
  in
  match cd.pcd_args, cd.pcd_res with
  | _, Some _ -> Location.raise_errorf ~loc:cd.pcd_loc "ocapi: %s is a GADT constructor" name
  | Pcstr_tuple [], None ->
    if
      Attribute.get Attributes.constructor_codec cd <> None
      || Attribute.get Attributes.discriminator cd <> None
    then
      Location.raise_errorf ~loc:cd.pcd_loc "ocapi: %s has no payload to encode" name;
    let project =
      Ast_builder.Default.pexp_function_cases ~loc
        (Ast_builder.Default.case ~lhs:(pconstr None) ~guard:None ~rhs:[%expr Some ()] :: others)
    in
    let prism =
      named
        [%expr Ocapi.Endpoint.Prism.make ~inject:(fun () -> [%e constr None]) ~project:[%e project]]
    in
    Ast_builder.Default.pexp_apply ~loc [%expr Ocapi.Endpoint.Response.empty]
      (((Labelled "status", status) :: description) @ [ Nolabel, prism ])
  | Pcstr_tuple [ ty ], None ->
    let codec =
      match Attribute.get Attributes.constructor_codec cd with
      | Some codec -> codec
      | None -> Names.conventional ~loc:cd.pcd_loc ~suffix:"codec" ty
    in
    let discriminator =
      match Attribute.get Attributes.discriminator cd with
      | Some (property, value) ->
        [
          ( Labelled "discriminator",
            [%expr
              [%e Ast_builder.Default.estring ~loc property],
              [%e Ast_builder.Default.estring ~loc value]] );
        ]
      | None -> []
    in
    let project =
      Ast_builder.Default.pexp_function_cases ~loc
        (Ast_builder.Default.case ~lhs:(pconstr (Some [%pat? x])) ~guard:None ~rhs:[%expr Some x]
        :: others)
    in
    let prism =
      named
        [%expr
          Ocapi.Endpoint.Prism.make
            ~inject:(fun x -> [%e constr (Some [%expr x])])
            ~project:[%e project]]
    in
    Ast_builder.Default.pexp_apply ~loc [%expr Ocapi.Endpoint.Response.json]
      (((Labelled "status", status) :: description)
      @ discriminator
      @ [ Nolabel, codec; Nolabel, prism ])
  | (Pcstr_tuple (_ :: _ :: _) | Pcstr_record _), None ->
    Location.raise_errorf ~loc:cd.pcd_loc
      "ocapi: %s takes one value; wrap several in a type with a codec" name

let derive ~ctxt (_, tds) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match tds with
  | [ { ptype_kind = Ptype_variant constructors; ptype_params = []; _ } ] ->
    let many = List.length constructors > 1 in
    let cases = constructors |> List.map (case ~loc ~many) in
    [ [%stri let responses = [%e Ast_builder.Default.elist ~loc cases]] ]
  | [ _ ] ->
    Location.raise_errorf ~loc "ocapi: [@@deriving responses] takes a variant without parameters"
  | _ -> Location.raise_errorf ~loc "ocapi: [@@deriving responses] takes one type"

let deriver : (structure_item list, rec_flag * type_declaration list) Deriving.Generator.t =
  Deriving.Generator.V2.make_noarg derive
