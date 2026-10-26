(* [[@@deriving provide ~path:"..."]]: a request record read from an HTTP request. *)

open Ppxlib

type location = Path | Query | Header | Cookie | Body | Custom of expression

let segments template =
  String.split_on_char '/' template
  |> List.filter (( <> ) "")
  |> List.filter_map (fun s ->
    let n = String.length s in
    if n >= 2 && s.[0] = '{' && s.[n - 1] = '}' then Some (String.sub s 1 (n - 2)) else None)

let location ~in_template (ld : label_declaration) =
  let found =
    List.filter_map Fun.id
      [
        Attribute.get Attributes.path ld |> Option.map (fun n -> Path, n);
        Attribute.get Attributes.query ld |> Option.map (fun n -> Query, n);
        Attribute.get Attributes.header ld |> Option.map (fun n -> Header, n);
        Attribute.get Attributes.cookie ld |> Option.map (fun n -> Cookie, n);
        Attribute.get Attributes.body ld |> Option.map (fun () -> Body, None);
        Attribute.get Attributes.provide ld |> Option.map (fun e -> Custom e, None);
      ]
  in
  let loc = ld.pld_loc in
  let wire = function Some n -> n | None -> ld.pld_name.txt in
  match found with
  | [ (where, name) ] -> where, wire name
  | [] when List.mem ld.pld_name.txt in_template -> Path, ld.pld_name.txt
  | [] -> Query, ld.pld_name.txt
  | _ :: _ :: _ ->
    Location.raise_errorf ~loc
      "ocapi: %s has more than one of path, query, header, cookie, body, provide" ld.pld_name.txt

let option_of (ty : core_type) =
  match ty.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ inner ]) -> Some inner
  | _ -> None

let value_of ~loc (ld : label_declaration) ty =
  match Attribute.get Attributes.value ld with
  | Some e -> e
  | None -> (
    match ty.ptyp_desc with
    | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> [%expr Ocapi.Endpoint.Value.string]
    | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> [%expr Ocapi.Endpoint.Value.int ()]
    | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> [%expr Ocapi.Endpoint.Value.bool]
    | _ -> Names.conventional ~loc ~suffix:"value" ty)

(* OpenAPI ignores header parameters of these names; [Endpoint.Header] refuses them too. *)
let reserved_headers = [ "authorization"; "content-type"; "accept" ]

let field_provider ~loc ~self ~in_template (ld : label_declaration) =
  Attributes.check_field ld;
  let where, name = location ~in_template ld in
  let field = ld.pld_name.txt in
  let get =
    [%expr
      fun (r : [%t self]) ->
        [%e Ast_builder.Default.pexp_field ~loc [%expr r] { txt = Lident field; loc }]]
  in
  let description = Doc.description ~loc ld.pld_attributes in
  let reject ~what provider =
    match Attribute.get Attributes.reject ld with
    | Some reject -> [%expr [%e provider] |> [%e reject]]
    | None ->
      let reason = Ast_builder.Default.estring ~loc what in
      [%expr
        [%e provider]
        |> Ocapi.Endpoint.Provide.require
             ~responses:[ 400, [%e reason] ]
             (fun _ -> Ocapi.Endpoint.Reply.empty 400)]
  in
  let only_for kinds attr label =
    match Attribute.get attr ld with
    | Some _ when not (List.mem where kinds) ->
      Location.raise_errorf ~loc "ocapi: [@%s] does not apply to %s" label field
    | Some _ | None -> ()
  in
  only_for [ Query; Header; Cookie ] Attributes.default "default";
  only_for [ Query; Header; Cookie; Body ] Attributes.reject "reject";
  only_for [ Body ] Attributes.field_codec "codec";
  let parameter module_ =
    let call kind =
      Ast_builder.Default.pexp_ident ~loc
        { txt = Ldot (Ldot (Ldot (Lident "Ocapi", "Endpoint"), module_), kind); loc }
    in
    let wire = Ast_builder.Default.estring ~loc name in
    let apply kind value get =
      Ast_builder.Default.pexp_apply ~loc (call kind)
        (description @ [ Nolabel, value; Nolabel, wire; Nolabel, get ])
    in
    match option_of ld.pld_type, Attribute.get Attributes.default ld with
    | Some _, Some _ -> Location.raise_errorf ~loc "ocapi: %s is optional and has a default" field
    | Some inner, None ->
      reject ~what:(name ^ " is invalid") (apply "optional" (value_of ~loc ld inner) get)
    | None, Some default ->
      let some = [%expr fun r -> Some ([%e get] r)] in
      [%expr
        [%e
          reject ~what:(name ^ " is invalid") (apply "optional" (value_of ~loc ld ld.pld_type) some)]
        |> Ocapi.Endpoint.Provide.map (Option.value ~default:[%e default])]
    | None, None ->
      reject ~what:(name ^ " is missing or invalid")
        (apply "required" (value_of ~loc ld ld.pld_type) get)
  in
  match where with
  | Path ->
    if option_of ld.pld_type <> None then
      Location.raise_errorf ~loc "ocapi: the path parameter %s cannot be optional" field;
    if not (List.mem name in_template) then
      Location.raise_errorf ~loc "ocapi: %s is not a parameter of the path template" name;
    Ast_builder.Default.pexp_apply ~loc [%expr Ocapi.Endpoint.Path.value]
      (description
      @ [
          Nolabel, value_of ~loc ld ld.pld_type;
          Nolabel, Ast_builder.Default.estring ~loc name;
          Nolabel, get;
        ])
  | Query -> parameter "Query"
  | Header ->
    if List.mem (String.lowercase_ascii name) reserved_headers then
      Location.raise_errorf ~loc
        "ocapi: the header %s is not a parameter; declare it as security or media" name;
    parameter "Header"
  | Cookie -> parameter "Cookie"
  | Body ->
    let codec =
      match Attribute.get Attributes.field_codec ld with
      | Some codec -> codec
      | None -> Names.conventional ~loc ~suffix:"codec" ld.pld_type
    in
    reject ~what:"The body is malformed" [%expr Ocapi.Endpoint.Body.json [%e codec] [%e get]]
  | Custom provider -> [%expr Ocapi.Endpoint.Provide.contramap [%e get] [%e provider]]

let path_arg = Deriving.Args.(arg "path" (estring __))

let derive ~ctxt (_, tds) path =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match tds with
  | [ ({ ptype_kind = Ptype_record fields; ptype_params = []; ptype_name; _ } as td) ] ->
    let self = Ast_builder.Default.ptyp_constr ~loc { txt = Lident ptype_name.txt; loc } [] in
    let in_template = Option.fold path ~none:[] ~some:segments in
    (match path with
    | None -> ()
    | Some template ->
      if not (String.starts_with ~prefix:"/" template) then
        Location.raise_errorf ~loc "ocapi: the path template %s does not start with /" template;
      if List.length (List.sort_uniq compare in_template) <> List.length in_template then
        Location.raise_errorf ~loc "ocapi: the path template %s names a parameter twice" template);
    let providers =
      fields |> List.map (fun ld -> ld, field_provider ~loc:ld.pld_loc ~self ~in_template ld)
    in
    let path_fields =
      providers
      |> List.filter_map (fun (ld, _) ->
        match location ~in_template ld with Path, name -> Some name | _ -> None)
    in
    Option.iter
      (fun template ->
        in_template
        |> List.iter (fun p ->
          if not (List.mem p path_fields) then
            Location.raise_errorf ~loc "ocapi: no field provides {%s} of %s" p template))
      path;
    let has_body = List.exists (fun (ld, _) -> fst (location ~in_template ld) = Body) providers in
    let record =
      Ast_builder.Default.pexp_record ~loc
        (fields
        |> List.map (fun ld ->
          { txt = Lident ld.pld_name.txt; loc }, Ast_builder.Default.evar ~loc ld.pld_name.txt))
        None
    in
    let body = [%expr ([%e record] : [%t self])] in
    let bindings =
      providers
      |> List.map (fun (ld, provider) ->
        Ast_builder.Default.value_binding ~loc
          ~pat:(Ast_builder.Default.pvar ~loc ld.pld_name.txt)
          ~expr:provider)
    in
    let letop =
      match bindings with
      | [] -> assert false (* A record has a field. *)
      | first :: rest ->
        let op name (vb : value_binding) =
          Ast_builder.Default.binding_op ~loc ~op:{ txt = name; loc } ~pat:vb.pvb_pat
            ~exp:vb.pvb_expr
        in
        Ast_builder.Default.pexp_letop ~loc
          (Ast_builder.Default.letop ~let_:(op "let+" first)
             ~ands:(List.map (op "and+") rest)
             ~body)
    in
    let summary, description = Doc.summary_and_description td.ptype_attributes in
    let some name value =
      Option.map
        (fun v ->
          [%stri
            let [%p Ast_builder.Default.pvar ~loc name] =
              Some [%e Ast_builder.Default.estring ~loc v]])
        value
      |> Option.to_list
    in
    let body_state =
      if has_body then
        [%stri type body = Ocapi.Endpoint.Provide.body_read]
      else
        [%stri type body = Ocapi.Endpoint.Provide.body_unread]
    in
    [ body_state; [%stri type doc = Ocapi.Endpoint.complete] ]
    @ Option.fold path ~none:[] ~some:(fun p ->
      [ [%stri let path = [%e Ast_builder.Default.estring ~loc p]] ])
    @ some "summary" summary @ some "description" description
    @ some "operation_id" (Names.operation_id (Expansion_context.Deriver.code_path ctxt))
    @ [
        [%stri
          let provide =
            let open Ocapi.Endpoint.Syntax in
            [%e letop]];
      ]
  | [ _ ] ->
    Location.raise_errorf ~loc "ocapi: [@@deriving provide] takes a record without parameters"
  | _ -> Location.raise_errorf ~loc "ocapi: [@@deriving provide] takes one type"

let deriver : (structure_item list, rec_flag * type_declaration list) Deriving.Generator.t =
  Deriving.Generator.V2.make Deriving.Args.(empty +> path_arg) derive
