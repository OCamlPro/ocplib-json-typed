# ocplib-json-typed

This library is a collection of type-aware JSON utilities for OCaml.

  - `Json_encoding` contains an `'a encoding` type that represents
    the JSON encoding of OCaml values of type `'a`, and a collection
    of combinators to build them. These encodings can be used to
    serialize / deserialize OCaml values to / from JSON
    documents. JSON schemas can also be produced automatically to
    produce documented, interoperable JSON formats.
  - `Json_schema` contains an OCaml intermediate representation for
    the JSON schema document grammar description language, along with
    translators to / from the concrete JSON schema format.
  - `Json_repr` contains various utilities to manipulate, deconstruct
    and convert JSON data.

The type of JSON documents handled by this library is directly
compatible with `ezjsonm`, but functions are provided to use `yojson`
instead. Thanks to polymorphic variants, this library does not depend
on any JSON library, so you are free to use whichever you want for
printing and parsing.

This is work in progress and the API may change. Let us know if you
rely on this library, so we can avoid breaking your code if possible.
