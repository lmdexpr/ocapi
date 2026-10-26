(* Doc comments (odoc markup) as OpenAPI text: a plain summary, a CommonMark description. *)

open Odoc_parser
module Ast = Odoc_parser.Ast

let value = Loc.value

(* A summary is plain text: the markup goes, its text stays. *)
let rec inline ~plain : Ast.inline_element -> string = function
  | `Space _ -> " "
  | `Word w -> w
  | `Code_span c -> if plain then c else "`" ^ c ^ "`"
  | `Raw_markup (_, _) -> ""
  | `Styled (style, xs) -> (
    let inner = inlines ~plain xs in
    match style with
    | _ when plain -> inner
    | `Bold -> "**" ^ inner ^ "**"
    | `Italic | `Emphasis -> "*" ^ inner ^ "*"
    | `Superscript | `Subscript -> inner)
  | `Reference (_, target, []) -> if plain then value target else "`" ^ value target ^ "`"
  | `Reference (_, _, text) -> inlines ~plain text
  | `Link (url, []) -> if plain then url else "<" ^ url ^ ">"
  | `Link (url, text) ->
    if plain then inlines ~plain text else "[" ^ inlines ~plain text ^ "](" ^ url ^ ")"
  | `Math_span m -> if plain then m else "$" ^ m ^ "$"

and inlines ~plain xs = xs |> List.map (fun x -> inline ~plain (value x)) |> String.concat ""

let indent prefix text =
  String.split_on_char '\n' text
  |> List.mapi (fun i line ->
    if i = 0 then
      prefix ^ line
    else if line = "" then
      line
    else
      String.make (String.length prefix) ' ' ^ line)
  |> String.concat "\n"

let fenced ?(language = "") content = "```" ^ language ^ "\n" ^ content ^ "\n```"

let rec block (b : Ast.nestable_block_element Loc.with_location) =
  match value b with
  | `Paragraph xs -> inlines ~plain:false xs
  | `Code_block { meta; content; _ } ->
    let language =
      Option.fold meta ~none:"" ~some:(fun (m : Ast.code_block_meta) -> value m.language)
    in
    fenced ~language (fst (codeblock_content (Loc.location content) (value content)))
  | `Verbatim s -> fenced (fst (verbatim_content (Loc.location b) s))
  | `Modules modules -> modules |> List.map (fun m -> "`" ^ value m ^ "`") |> String.concat ", "
  | `List (kind, _, items) ->
    items
    |> List.mapi (fun i item ->
      let marker =
        match kind with `Unordered -> "- " | `Ordered -> string_of_int (i + 1) ^ ". "
      in
      indent marker (blocks item))
    |> String.concat "\n"
  | `Table ((grid, _), _) -> (
    let row cells =
      "| "
      ^ (cells
        |> List.map (fun (content, _) ->
          blocks content |> String.map (function '\n' -> ' ' | c -> c))
        |> String.concat " | ")
      ^ " |"
    in
    match grid with
    | [] -> ""
    | first :: rest ->
      let rule = "|" ^ String.concat "|" (List.map (fun _ -> " --- ") first) ^ "|" in
      String.concat "\n" ((row first :: [ rule ]) @ List.map row rest))
  | `Math_block m -> "$$\n" ^ m ^ "\n$$"
  | `Media (_, href, alt, _) -> (
    match value href with `Link url -> "![" ^ alt ^ "](" ^ url ^ ")" | `Reference _ -> alt)

and blocks xs = xs |> List.map block |> List.filter (( <> ) "") |> String.concat "\n\n"

(* Tags ([@param], [@since], ...) describe the OCaml value, not the HTTP operation. *)
let nestable (b : Ast.block_element Loc.with_location) =
  match value b with
  | #Ast.nestable_block_element as n -> Some (Loc.same b n)
  | `Heading (level, _, xs) ->
    Some
      (Loc.same b
         (`Paragraph
            [
              Loc.same b
                (`Word (String.make (max 1 (min 6 level)) '#' ^ " " ^ inlines ~plain:false xs));
            ]))
  | `Tag _ -> None

let parse ~(loc : Ppxlib.location) text =
  parse_comment ~location:loc.loc_start ~text |> ast |> List.filter_map nestable

let some = function "" -> None | s -> Some s

(** The whole comment as CommonMark. *)
let description ~loc text = some (blocks (parse ~loc text))

(** The first paragraph as plain text, and the rest as CommonMark. *)
let summary_and_description ~loc text =
  match parse ~loc text with
  | ({ value = `Paragraph xs; _ } : Ast.nestable_block_element Loc.with_location) :: rest ->
    some (String.trim (inlines ~plain:true xs)), some (blocks rest)
  | all -> None, some (blocks all)
