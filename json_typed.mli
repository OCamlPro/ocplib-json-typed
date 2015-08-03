(** JSON structure description using dependently typed combinators. *)

(************************************************************************)
(*  ocplib-json-typed-utils                                             *)
(*                                                                      *)
(*    Copyright 2014 OCamlPro                                           *)
(*                                                                      *)
(*  This file is distributed under the terms of the GNU Lesser General  *)
(*  Public License as published by the Free Software Foundation; either *)
(*  version 2.1 of the License, or (at your option) any later version,  *)
(*  with the OCaml static compilation exception.                        *)
(*                                                                      *)
(*  ocp-read is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*  GNU General Public License for more details.                        *)
(*                                                                      *)
(************************************************************************)

open Json_repr

(** {2 Dependent types describing JSON document structures} *) (***************)

(** A codec between an OCaml data type (the first parameter) and a JSON
    representation. The second parameter is to mark [codec]s that
    can be used to describe toplevel json documents. *)
type ('a, 'b) codec constraint 'b = [< value]

(** Builds a json value from an OCaml value and a codec encoding. *)
val construct : ('t, [< value ] as 'k) codec -> 't -> 'k

(** Reads an OCaml value from a JSON value and a codec encoding.
    May raise [Cannot_destruct]. *)
val destruct : ('t, [< value ]) codec -> [< value ] -> 't

(** {2 JSON type combinators for simple immediates} *) (***********************)

(** A codec between an OCaml unit and any (ignored) JSON. *)
val unit : (unit, value) codec

(** A codec between an OCaml unit and a JSON null. *)
val null : (unit, [ `Null ]) codec

(** A codec between an OCaml unit and an empty JSON object. *)
val empty : (unit, [ `O of (string * value) list ]) codec

(** A codec between an OCaml int and a JSON number. *)
val int : (int, [ `Float of float ]) codec

(** A codec between an OCaml int32 and a JSON number. *)
val int32 : (int32, [ `Float of float ]) codec

(** A codec between an OCaml boolean and a JSON one. *)
val bool : (bool, [ `Bool of bool ]) codec

(** A codec between an OCaml string and a JSON one. *)
val string : (string, [ `String of string ]) codec

(** A fixed map between OCaml values and JSON strings. *)
val string_enum : (string * 'a) list -> ('a, [ `String of string ]) codec

(** A codec between an OCaml mutable string and a JSON string. *)
val bytes : (bytes, [ `String of string ]) codec

(** A codec between an OCaml float and a JSON number. *)
val float : (float, [ `Float of float ]) codec

(** A codec between an OCaml option and a nullable JSON value. *)
val option : ('a, [< value ]) codec -> ('a option, value ) codec

(** {2 JSON type combinators for objects} *) (*********************************)

(** A first class handle to a JSON field. *)
type 'a field

(** A required field of a given its type. *)
val req : string -> ('t, _) codec -> 't field

(** An optional field of a given type, using an OCaml [option]. *)
val opt : string -> ('t, _) codec -> 't option field

(** A codec between an OCaml value and a singleton object. *)
val obj1 :
  'f1 field ->
  ('f1, [ `O of (string * value) list ]) codec

(** A codec between an OCaml pair and a JSON object with two fields. *)
val obj2 :
  'f1 field ->
  'f2 field ->
  ('f1 * 'f2, [ `O of (string * value) list ]) codec

(** A codec between an OCaml triplet and a JSON object with three fields. *)
val obj3 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  ('f1 * 'f2 * 'f3, [ `O of (string * value) list ]) codec

(** A codec between an OCaml quadruplet and a JSON object with four fields. *)
val obj4 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  ('f1 * 'f2 * 'f3 * 'f4, [ `O of (string * value) list ]) codec

(** A codec between an OCaml quintuplet and a JSON object with five fields. *)
val obj5 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5, [ `O of (string * value) list ]) codec

(** A codec between an OCaml sextuplet and a JSON object with six fields. *)
val obj6 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  'f6 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6, [ `O of (string * value) list ]) codec

(** Merge two object [codec]s. For describing heavyweight objects with
    a lot of fields. The ocaml type is a pair of tuples, but the JSON
    object is flat. *)
val merge_objs :
  ('o1, [ `O of (string * value) list ]) codec ->
  ('o2, [ `O of (string * value) list ]) codec ->
  ('o1 * 'o2, [ `O of (string * value) list ]) codec

(** {2 JSON type combinators for arrays} *) (**********************************)

(** A higher order codec between an OCaml array and a JSON one. *)
val array :
  ('a, _) codec ->
  ('a array, [ `A of value list ]) codec

(** A higher order codec between an OCaml array and a JSON one. *)
val list :
  ('a, _) codec ->
  ('a list, [ `A of value list ]) codec

(** A codec between an OCaml value and a singleton array. *)
val tup1 :
  ('f1, _) codec ->
  ('f1, [ `A of value list ]) codec

(** A codec between an OCaml pair and a JSON array with two cells. *)
val tup2 :
  ('f1, _) codec ->
  ('f2, _) codec ->
  ('f1 * 'f2, [ `A of value list ]) codec

(** A codec between an OCaml triplet and a JSON array with three cells. *)
val tup3 :
  ('f1, _) codec ->
  ('f2, _) codec ->
  ('f3, _) codec ->
  ('f1 * 'f2 * 'f3, [ `A of value list ]) codec

(** A codec between an OCaml quadruplet and a JSON array with four cells. *)
val tup4 :
  ('f1, _) codec ->
  ('f2, _) codec ->
  ('f3, _) codec ->
  ('f4, _) codec ->
  ('f1 * 'f2 * 'f3 * 'f4, [ `A of value list ]) codec

(** A codec between an OCaml quintuplet and a JSON array with five cells. *)
val tup5 :
  ('f1, _) codec ->
  ('f2, _) codec ->
  ('f3, _) codec ->
  ('f4, _) codec ->
  ('f5, _) codec ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5, [ `A of value list ]) codec

(** A codec between an OCaml sextuplet and a JSON array with six cells. *)
val tup6 :
  ('f1, _) codec ->
  ('f2, _) codec ->
  ('f3, _) codec ->
  ('f4, _) codec ->
  ('f5, _) codec ->
  ('f6, _) codec ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6, [ `A of value list ]) codec

(** Merge two tuple [codec]s. For describing heavyweight arrays with a
    lot of cells. The ocaml type is a pair of tuples, but the JSON
    array is flat, with the elements of the first tuple before the
    ones of the second. The two first args must result from a call to
    [tup[1-6]] otherwise a run-time error will arise (this behaviour
    may change in a later release). *)
val merge_tups :
  ('a1, [ `A of value list ]) codec ->
  ('a2, [ `A of value list ]) codec ->
  ('a1 * 'a2, [ `A of value list ]) codec

(** {2 JSON type combinators for unions} *) (**********************************)

(** A case for describing union types using {!union} ans {!case}. *)
type 't case

(** To be used inside a {!union}. Takes a [codec] for a specific
    case, and a converter to and from a type common to all cases
    (['t]). Usually, it consists in boxing / deboxing the specific
    data in an OCaml sum type contructor. *)
val case : ('a, _) codec -> ('t -> 'a option) -> ('a -> 't) -> 't case

(** A utility to build destructors for custom encoded sum types. *)
val union : 't case list -> ('t, value) codec

(** {2 JSON generic type combinators} *) (*************************************)

(** A codec between a custom OCaml type and a JSON representation,
    given both custom conversion functions and a JSON schema description.
    The second transformer function can [raise (Cannot_destruct ([], "message"))]
    to indicate an error, which will be relocated correctly. *)
val custom :
  ('t -> value) ->
  (value -> 't) ->
  schema: Json_schema.schema ->
  ('t, value) codec

(** An adapter codec, with an optional handwritten schema.
    The second transformer function can [raise (Cannot_destruct ([], "message"))]
    to indicate an error, which will be relocated correctly. *)
val conv :
  ('a -> 'b) ->
  ('b -> 'a) ->
  ?schema: Json_schema.schema ->
  ('b, [< value ] as 'k) codec ->
  ('a, 'k) codec

(** A fixpoint combinator. Links a recursive OCaml type to an internal
    JSON schema reference, by allowing to use the codec inside its own
    definition. The first parameter is a path, that must be unique and
    respect the format of {!Json_schema.add_definition}. It is used to
    encode the recursivity as a named reference in the JSON schema.

    Here is an example to turn a standard OCaml list into either
    ["nil"] for [[]] or [{"hd":hd,"tl":tl}] for [hd::tl].

    {[ let reclist itemcodec =
         mu "list" @@ fun self ->
         union
           [ case (string_enum [ "nil", () ])
               (function [] -> Some () | _ :: _ -> None)
               (fun () -> []) ;
             case (obj2 (req "hd" itemcodec) (req "tl" self))
               (function hd :: tl -> Some (hd, tl) | [] -> None)
               (fun (hd, tl) -> hd :: tl) ]) ]} *)
val mu : string -> (('a, 'k) codec -> ('a, 'k) codec) -> ('a, 'k) codec

(** A raw JSON value. *)
val any_value : (value, value) codec

(** A raw JSON document. *)
val any_document : (document, document) codec

(** The codec of a JSON schema, linked to its OCaml definiton. *)
val any_schema : (Json_schema.schema, value) codec

(** {2 Exporting [codec]s as JSON schemas} *) (********************************)

(** Describe a codec encoding in JSON schema format, may raise
    [Invalid_argument "codecd.schema"] if a bad [custom] schema
    is present in the definition. *)
val schema : ('t, _) codec -> Json_schema.schema

(** Annotate a type with a title and description for the JSON schema. *)
val describe :
  ?title:string ->
  ?description:string ->
  ('t, 'k) codec ->
  ('t, 'k) codec

(** Name a definition so its occurences can be shared in the JSON
    schema.  The first parameter is a path, that must be unique and
    respect the format of {!Json_schema.add_definition}. *)
val def : string -> ('t, 'k) codec -> ('t, 'k) codec

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

