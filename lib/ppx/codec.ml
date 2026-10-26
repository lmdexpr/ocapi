(* [[@@deriving ocapi]]: a codec of the functions ppx_yojson_conv and ppx_deriving_jsonschema derive. *)

open Ppxlib

let derive ~ctxt (_, tds) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  tds
  |> List.map @@ fun td ->
     let t = td.ptype_name.txt in
     let prefixed suffix = if t = "t" then suffix else t ^ "_" ^ suffix in
     let name =
       if t <> "t" then
         t
       else
         match
           List.rev (Code_path.submodule_path (Expansion_context.Deriver.code_path ctxt))
         with
         | m :: _ -> String.lowercase_ascii m
         | [] -> Names.snake (Code_path.main_module_name (Expansion_context.Deriver.code_path ctxt))
     in
     [%stri
       let [%p Ast_builder.Default.pvar ~loc (prefixed "codec")] =
         Ocapi_jsonschema.codec
           ~name:[%e Ast_builder.Default.estring ~loc name]
           [%e Ast_builder.Default.evar ~loc (prefixed "jsonschema")]
           [%e Ast_builder.Default.evar ~loc ("yojson_of_" ^ t)]
           [%e Ast_builder.Default.evar ~loc (t ^ "_of_yojson")]]

let deriver : (structure_item list, rec_flag * type_declaration list) Deriving.Generator.t =
  Deriving.Generator.V2.make_noarg derive
