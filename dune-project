(lang dune 2.7)

(cram enable)
(name ttt)
(allow_approximate_merlin)
(generate_opam_files false)
(version dev)
(formatting (enabled_for ocaml reason))

(package
 (name ocplib-json-typed)
 (synopsis "Libraries for reliable manipulation JSON objects")
 (description "Libraries for reliable manipulation JSON objects")
 (depends
   (ocaml (>= 4.03.0))
   (dune (>= 1.7))
   (uri (>= 1.9.0))
 )
)

(package
 (name ocplib-json-typed-browser)
 (synopsis "Libraries for reliable manipulation JSON objects (browser support)")
 (description "Libraries for reliable manipulation JSON objects (browser support)")
 (depends
   (ocaml (>= 4.03.0))
   (dune (>= 1.7))
   (ocplib-json-typed (= dev))
   (js_of_ocaml (>= 3.3.0))
 )
)

(package
 (name ocplib-json-typed-bson)
 (synopsis "Libraries for reliable manipulation JSON objects (BSON)")
 (description "Libraries for reliable manipulation JSON objects (BSON)")
 (depends
   (ocaml (>= 4.03.0))
   (dune (>= 1.7))
   (ocplib-json-typed (= dev))
   (ocplib-endian (>= 1.0))
 )
)
