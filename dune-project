(lang dune 2.0)
(name ocplib-json-typed)

(source (github ocamlpro/ocplib-json-typed))
(authors "Benjamin Canou <benjamin@ocamlpro.com>")
(maintainers "Benjamin Canou <benjamin@ocamlpro.com>")

(package
 (name ocplib-json-typed)
 (synopsis "Libraries for reliable manipulation JSON objects (browser support)")
 (depends
  (ocaml (>= 4.03.0))
  (dune (and :build (>= 1.7)))
  (uri (>= 1.9.0))))

(package
 (name ocplib-json-typed-bson)
 (synopsis "Libraries for reliable manipulation JSON objects (BSON)")
 (depends ocplib-json-typed ocplib-endian))

(package
 (name ocplib-json-typed-browser)
 (synopsis "Libraries for reliable manipulation JSON objects (browser support)")
 (depends ocplib-json-typed js_of_ocaml))
