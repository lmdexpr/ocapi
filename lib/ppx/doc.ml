(* Doc comments, read from the attributes the compiler turns them into. *)

open Ppxlib

let doc attributes =
  attributes
  |> List.find_map (fun attribute ->
    match attribute.attr_name.txt, attribute.attr_payload with
    | ( ("ocaml.doc" | "doc"),
        PStr
          [
            {
              pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (s, loc, _)); _ }, _);
              _;
            };
          ] ) ->
      Some (s, loc)
    | _ -> None)

(* The whole comment as [~description], for a parameter or a response. *)
let description ~loc attributes =
  match Option.bind (doc attributes) (fun (text, loc) -> Odoc_markdown.description ~loc text) with
  | None -> []
  | Some s -> [ Labelled "description", Ast_builder.Default.estring ~loc s ]

(* The first paragraph is the summary, the rest the description. *)
let summary_and_description attributes =
  match doc attributes with
  | None -> None, None
  | Some (text, loc) -> Odoc_markdown.summary_and_description ~loc text
