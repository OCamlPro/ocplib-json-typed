(** JSON structure description using dependently typed combinators. *)

(************************************************************************)
(*  ocplib-json-typed                                                   *)
(*                                                                      *)
(*    Copyright 2014 OCamlPro                                           *)
(*                                                                      *)
(*  This file is distributed under the terms of the GNU Lesser General  *)
(*  Public License as published by the Free Software Foundation; either *)
(*  version 2.1 of the License, or (at your option) any later version,  *)
(*  with the OCaml static compilation exception.                        *)
(*                                                                      *)
(*  ocplib-json-typed is distributed in the hope that it will be useful,*)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*  GNU General Public License for more details.                        *)
(*                                                                      *)
(************************************************************************)

open Json_repr

(** {2 Dependent types describing JSON document structures} *) (***************)

(** An encoding between an OCaml data type (the parameter) and a JSON
    representation. To be built using the predefined combinators
    provided by this module.

    For instance, here is an encoding, of type [(int * string)
    encoding], mapping values of type [int * string] to JSON objects
    with a field [code] of whose value is a number and a field
    [message] whose value is a string.

    [let enc = obj2 (req "code" int) (req "message" string)]

    This encoding serves three purposes:

      1. Output an OCaml value of type ['a] to an intermediate JSON represntation
         using {!construct}. To be printed to actual JSON using an external
         library. The intermediate structure is compatible with {!Ezjsonm.value},
         but {!Json_repr} provides converters for use with {!Yojson.Safe.json}.
      2. Input a JSON intermediate structure (already parsed with an external
         library) to produce an OCaml value of type ['a].
      3. Describe this encoding in JSON-schema format for inter-operability:
         you describe the encoding of your internal types, and obtain
         machine-readable descriptions of the formats as a byproduct.
         Specific documentation combinators are provided for that purpose. *)
type 'a encoding

(** Builds a json value from an OCaml value and an encoding. *)
val construct : 't encoding -> 't -> [> value ]

(** Reads an OCaml value from a JSON value and an encoding.
    May raise [Cannot_destruct]. *)
val destruct : 't encoding -> [< value ] -> 't

(** {2 JSON type combinators for simple immediates} *) (***********************)

(** An encoding of an OCaml unit by any (ignored) JSON. *)
val unit : unit encoding

(** An encoding of an OCaml unit by a JSON null. *)
val null : unit encoding

(** An encoding of an OCaml unit by an empty JSON object. *)
val empty : unit encoding

(** An encoding of an OCaml int by a JSON number. *)
val int : int encoding

(** An encoding of an OCaml int32 by a JSON number. *)
val int32 : int32 encoding

(** An encoding of an OCaml boolean by a JSON one. *)
val bool : bool encoding

(** An encoding of an OCaml string by a JSON one. *)
val string : string encoding

(** An encoding of a closed set of OCaml values by JSON strings. *)
val string_enum : (string * 'a) list -> 'a encoding

(** An encoding of an OCaml mutable string by a JSON string. *)
val bytes : bytes encoding

(** An encoding of an OCaml float by a JSON number. *)
val float : float encoding

(** An encoding of an OCaml option by a nullable JSON value. *)
val option : 'a encoding -> 'a option encoding

(** {2 JSON type combinators for objects} *) (*********************************)

(** A first class handle to a JSON field. *)
type 'a field

(** A required field of a given its type. *)
val req : ?title:string -> ?description:string -> string -> 't encoding -> 't field

(** An optional field of a given type, using an OCaml [option]. *)
val opt : ?title:string -> ?description:string -> string -> 't encoding -> 't option field

(** An optional field of a given type, ommited when equal to a default value. *)
val dft : ?title:string -> ?description:string -> string -> 't encoding -> 't -> 't field

(** An encoding of an OCaml value by a singleton object. *)
val obj1 :
  'f1 field ->
  'f1 encoding

(** An encoding of an OCaml pair by a JSON object with two fields. *)
val obj2 :
  'f1 field ->
  'f2 field ->
  ('f1 * 'f2) encoding

(** An encoding of an OCaml triplet by a JSON object with three fields. *)
val obj3 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  ('f1 * 'f2 * 'f3) encoding

(** An encoding of an OCaml quadruplet by a JSON object with four fields. *)
val obj4 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  ('f1 * 'f2 * 'f3 * 'f4) encoding

(** An encoding of an OCaml quintuplet by a JSON object with five fields. *)
val obj5 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding

(** An encoding of an OCaml sextuplet by a JSON object with six fields. *)
val obj6 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  'f6 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding

(** Merge two object [encoding]s. For describing heavyweight objects with
    a lot of fields. The ocaml type is a pair of tuples, but the JSON
    object is flat. Both arguments must be object encodings,
    otherwise a future {!construct}, {!destruct} or {!schema} will fail
    with [Invalid_argument]. *)
val merge_objs :
  'o1 encoding ->
  'o2 encoding ->
  ('o1 * 'o2) encoding

(** {2 JSON type combinators for arrays} *) (**********************************)

(** An encoding of an OCaml array by a JSON one. *)
val array :
  'a encoding ->
  'a array encoding

(** An encoding of an OCaml list by a JSON one. *)
val list :
  'a encoding ->
  'a list encoding

(** An encoding of an OCaml associative list by a JSON object. *)
val assoc :
  'a encoding ->
  (string * 'a) list encoding

(** An encoding of an OCaml value by a singleton array. *)
val tup1 :
  'f1 encoding ->
  'f1 encoding

(** An encoding of an OCaml pair by a JSON array with two cells. *)
val tup2 :
  'f1 encoding ->
  'f2 encoding ->
  ('f1 * 'f2) encoding

(** An encoding of an OCaml triplet by a JSON array with three cells. *)
val tup3 :
  'f1 encoding ->
  'f2 encoding ->
  'f3 encoding ->
  ('f1 * 'f2 * 'f3) encoding

(** An encoding of an OCaml quadruplet by a JSON array with four cells. *)
val tup4 :
  'f1 encoding ->
  'f2 encoding ->
  'f3 encoding ->
  'f4 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4) encoding

(** An encoding of an OCaml quintuplet by a JSON array with five cells. *)
val tup5 :
  'f1 encoding ->
  'f2 encoding ->
  'f3 encoding ->
  'f4 encoding ->
  'f5 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding

(** An encoding of an OCaml sextuplet by a JSON array with six cells. *)
val tup6 :
  'f1 encoding ->
  'f2 encoding ->
  'f3 encoding ->
  'f4 encoding ->
  'f5 encoding ->
  'f6 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding

(** Merge two tuple [encoding]s. For describing heavyweight arrays with a
    lot of cells. The ocaml type is a pair of tuples, but the JSON
    array is flat, with the elements of the first tuple before the
    ones of the second. Both arguments must be tuple encodings,
    otherwise a future {!construct}, {!destruct} or {!schema} will fail
    with [Invalid_argument]. *)
val merge_tups :
  'a1 encoding ->
  'a2 encoding ->
  ('a1 * 'a2) encoding

(** {2 JSON type combinators for unions} *) (**********************************)

(** A case for describing union types using {!union} ans {!case}. *)
type 't case

(** To be used inside a {!union}. Takes a [encoding] for a specific
    case, and a converter to and from a type common to all cases
    (['t]). Usually, it consists in boxing / deboxing the specific
    data in an OCaml sum type contructor. *)
val case : 'a encoding -> ('t -> 'a option) -> ('a -> 't) -> 't case

(** A utility to build destructors for custom encoded sum types. *)
val union : 't case list -> 't encoding

(** {2 JSON generic type combinators} *) (*************************************)

(** An encoding of a custom OCaml type by a JSON representation,
    given both custom conversion functions and a JSON schema description.
    The second transformer function can [raise (Cannot_destruct ([], "message"))]
    to indicate an error, which will be relocated correctly. *)
val custom :
  ('t -> value) ->
  (value -> 't) ->
  schema: Json_schema.schema ->
  't encoding

(** An encoding adapter, with an optional handwritten schema.
    The second transformer function can [raise (Cannot_destruct ([], "message"))]
    to indicate an error, which will be relocated correctly. *)
val conv :
  ('a -> 'b) ->
  ('b -> 'a) ->
  ?schema: Json_schema.schema ->
  'b encoding ->
  'a encoding

(** A fixpoint combinator. Links a recursive OCaml type to an internal
    JSON schema reference, by allowing to use the encoding inside its
    own definition. The first parameter is a path, that must be unique
    and respect the format of {!Json_schema.add_definition}. It is
    used to encode the recursivity as a named reference in the JSON
    schema.

    Here is an example to turn a standard OCaml list into either
    ["nil"] for [[]] or [{"hd":hd,"tl":tl}] for [hd::tl].

    {[ let reclist itemencoding =
         mu "list" @@ fun self ->
         union
           [ case (string_enum [ "nil", () ])
               (function [] -> Some () | _ :: _ -> None)
               (fun () -> []) ;
             case (obj2 (req "hd" itemencoding) (req "tl" self))
               (function hd :: tl -> Some (hd, tl) | [] -> None)
               (fun (hd, tl) -> hd :: tl) ]) ]} *)
val mu : string -> ('a encoding -> 'a encoding) -> 'a encoding

(** A raw JSON value. *)
val any_value : value encoding

(** The encoding of a JSON schema, linked to its OCaml definiton. *)
val any_schema : Json_schema.schema encoding

(** {2 Exporting [encoding]s as JSON schemas} *) (********************************)

(** Describe an encoding in JSON schema format.
    May raise {!Bad_schema}. *)
val schema : 't encoding -> Json_schema.schema

(** Annotate a type with a title and description for the JSON schema. *)
val describe :
  ?title:string ->
  ?description:string ->
  't encoding ->
  't encoding

(** Name a definition so its occurences can be shared in the JSON
    schema.  The first parameter is a path, that must be unique and
    respect the format of {!Json_schema.add_definition}. *)
val def : string -> 't encoding -> 't encoding

(** {2 Errors} *) (************************************************************)

(** Exception raised by destructors, with the location in the original
    JSON structure and the specific error. *)
exception Cannot_destruct of (path * exn)

(** Unexpected kind of data encountered (w/ the expectation). *)
exception Unexpected of string * string

(** Some {!union} couldn't be destructed, w/ the reasons for each {!case}. *)
exception No_case_matched of exn list

(** Array of unexpected size encountered  (w/ the expectation). *)
exception Bad_array_size of int * int

(** Missing field in an object. *)
exception Missing_field of string

(** Supernumerary field in an object. *)
exception Unexpected_field of string

(** Bad custom schema encountered. *)
exception Bad_schema of exn

(** Produces a human readable version of an error. *)
val print_error
  : ?print_unknown: (Format.formatter -> exn -> unit) ->
  Format.formatter -> exn -> unit

