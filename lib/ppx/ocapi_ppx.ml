open Ppxlib

let () =
  ignore @@ Deriving.add "provide" ~str_type_decl:Provide.deriver;
  ignore @@ Deriving.add "responses" ~str_type_decl:Responses.deriver;
  ignore @@ Deriving.add "ocapi" ~str_type_decl:Codec.deriver
