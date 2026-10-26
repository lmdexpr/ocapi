(* The names the derivers give and the names they expect by convention. *)

open Ppxlib

let words name = String.split_on_char '_' name |> List.filter (( <> ) "")

(* [Items.Get] in [items_api.ml] is [itemsGet]; a module named [Spec] is the declaration's own. *)
let operation_id code_path =
  match Code_path.submodule_path code_path |> List.filter (( <> ) "Spec") with
  | [] -> None
  | modules ->
    let id =
      modules |> List.concat_map words |> List.map String.capitalize_ascii |> String.concat ""
    in
    Some (String.uncapitalize_ascii id)

(* A value of type [item] is [item_suffix]; of type [Item.t], [Item.suffix]. *)
let conventional ~loc ~suffix (ty : core_type) =
  match ty.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "t"; _ }, []) -> Ast_builder.Default.evar ~loc suffix
  | Ptyp_constr ({ txt = Lident name; _ }, []) -> Ast_builder.Default.evar ~loc (name ^ "_" ^ suffix)
  | Ptyp_constr ({ txt = Ldot (m, "t"); _ }, []) ->
    Ast_builder.Default.pexp_ident ~loc { txt = Ldot (m, suffix); loc }
  | Ptyp_constr ({ txt = Ldot (m, name); _ }, []) ->
    Ast_builder.Default.pexp_ident ~loc { txt = Ldot (m, name ^ "_" ^ suffix); loc }
  | _ ->
    Location.raise_errorf ~loc "ocapi: no %s by convention for this type; give one with [@%s ...]"
      suffix suffix

let snake name =
  name |> String.to_seq
  |> Seq.fold_left
       (fun acc c ->
         match c with
         | 'A' .. 'Z' when acc <> "" -> acc ^ "_" ^ String.make 1 (Char.lowercase_ascii c)
         | c -> acc ^ String.make 1 (Char.lowercase_ascii c))
       ""
