(lang dune 3.23)

(implicit_transitive_deps false)

(generate_opam_files true)

(name ocapi)

(source
 (github lmdexpr/ocapi))

(license MIT)

(authors "Yuki Tajiri")

(maintainers "Yuki Tajiri <lmdexpr@gmail.com>")

(package
 (name ocapi)
 (synopsis
  "Declare an HTTP endpoint once, derive the server, the client and the OpenAPI document")
 (description
  "An HTTP endpoint as a value: method, path, parameters, request body and responses.\nServer routing, client requests and the OpenAPI document are derived from it.\nTransports are effect handlers; this package depends on yojson only.")
 (depends
  (ocaml
   (>= 5.3.0))
  dune
  yojson
  (alcotest :with-test)
  (ocamlformat :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)))

(package
 (name ocapi-cohttp-eio)
 (synopsis "Cohttp-eio adapters for ocapi")
 (description
  "Serves ocapi routes with a cohttp-eio server and handles the ocapi client effect with a\ncohttp-eio client.")
 (depends
  (ocaml
   (>= 5.3.0))
  dune
  (ocapi
   (= :version))
  eio
  cohttp-eio
  http
  uri
  yojson
  (eio_main :with-test)
  (alcotest :with-test)))

(package
 (name ocapi-openapi)
 (synopsis "OpenAPI 3.1 documents of ocapi endpoints")
 (description
  "Derives the OpenAPI 3.1 document of ocapi endpoint values. Only endpoints whose every part\nhas a schema are documented, which the types check.")
 (depends
  (ocaml
   (>= 5.3.0))
  dune
  (ocapi
   (= :version))
  yojson
  (alcotest :with-test)))

(package
 (name ocapi-jsonschema)
 (synopsis "Use ppx_deriving_jsonschema schemas in ocapi endpoints")
 (description
  "Codecs and body providers taking the schemas ppx_deriving_jsonschema derives.")
 (depends
  (ocaml
   (>= 5.3.0))
  dune
  (ocapi
   (= :version))
  (ocapi-openapi
   (= :version))
  (ppx_deriving_jsonschema
   (>= 0.0.7))
  yojson
  (alcotest :with-test)))

(package
 (name ocapi-ppx)
 (synopsis "Derive ocapi endpoint declarations from types")
 (description
  "[@@deriving provide] reads a request record from an HTTP request, [@@deriving responses] sends\na result variant, [@@deriving ocapi] makes a codec of derived JSON functions; doc comments\nbecome descriptions. The output is ordinary ocapi combinators.")
 (depends
  (ocaml
   (>= 5.3.0))
  dune
  (ocapi
   (= :version))
  (ppxlib
   (>= 0.36.0))
  (odoc-parser
   (>= 3.0.0))
  (ocapi-openapi
   (and
    :with-test
    (= :version)))
  (ocapi-jsonschema
   (and
    :with-test
    (= :version)))
  (ppx_deriving_jsonschema :with-test)
  (ppx_yojson_conv :with-test)
  (alcotest :with-test)))
