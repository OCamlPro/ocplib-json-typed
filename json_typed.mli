(** JSON structure description using dependently typed combinators. *)

(** {2 In memory JSON document representation} ********************************)

(** A JSON document, that cannot be an immediate. *)
type document =
  [ `O of (string * value) list
    (** An object [{ "name": value, ...  }], with UTF-8 encoded names. *)
  | `A of value list
    (** An array [[ value, ... ]] .*) ]

(** A non toplevel JSON value, structure or immediate. *)
and value =
  [ `O of (string * value) list
    (** Cf. {!document}. *)
  | `A of value list
    (** Cf. {!document}. *)
  | `Bool of bool
    (** A JS boolean [true] or [false]. *)
  | `Float of float
    (** A floating point number (double precision). *)
  | `String of string
    (** An UTF-8 encoded string. *)
  | `Null
    (** The [null] constant. *) ]

(** {2 Paths in JSON documents} ***********************************************)

(** An abstract type for paths, independent from concrete formats. *)
type path = path_item list

and path_item =
  [ `Field of string
  | `Index of int
  | `Star ]

(** {2 Dependent types describing JSON document structures} *******************)

(** A codec between an OCaml data type (the first parameter) and a JSON
    representation. The second parameter is to mark [codec]s that
    can be used to describe toplevel json documents. *)
type ('a, 'b) codec constraint 'b = [< value]

(** Builds a json value from an OCaml value and a codec encoding. *)
val construct : ('t, [< value ] as 'k) codec -> 't -> 'k

(** Error descriptions.*)
type error =
  | Unexpected of string * string
  (** Unexpected kind of data encountered (w/ the expectation). *)
  | No_case_matched of (path * error) list
  (** Some {!union} couldn't be destructed, w/ the reasons for each {!case}. *)
  | Bad_array_size of int * int
  (** Array of unexpected size encountered  (w/ the expectation). *)
  | Missing_field of string
  (** Missing field in an object. *)
  | Unexpected_field of string
  (** Supernumerary field in an object. *)

(** Exception raised by [codec] directed destructors, with the
    location in the original JSON structure and a description. *)
exception Cannot_destruct of (path * error)

(** Produces a human readable version of an error. *)
val print_error : Format.formatter -> (path * error) -> unit

(** Reads an OCaml value from a JSON value and a codec encoding.
    May raise [Cannot_destruct]. *)
val destruct : ('t, [< value ]) codec -> [< value ] -> 't

(** {2 JSON type combinators for simple immediates} ***************************)

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

(** {2 JSON type combinators for objects} *************************************)

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

(** {2 JSON type combinators for arrays} **************************************)

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

(** {2 JSON type combinators for unions} **************************************)

(** A case for describing union types using {!union} ans {!case}. *)
type 't case

(** To be used inside a {!union}. Takes a [codec] for a specific
    case, and a converter to and from a type common to all cases
    (['t]). Usually, it consists in boxing / deboxing the specific
    data in an OCaml sum type contructor. *)
val case : ('a, _) codec -> ('t -> 'a option) -> ('a -> 't) -> 't case

(** A utility to build destructors for custom encoded sum types. *)
val union : 't case list -> ('t, value) codec

(** {2 JSON generic type combinators} *****************************************)

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
    JSON schema reference (using the first parameter as name). *)
val mu : string -> (('a, 'k) codec -> ('a, 'k) codec) -> ('a, 'k) codec

(** A raw JSON value. *)
val any_value : (value, value) codec

(** A raw JSON document. *)
val any_document : (document, document) codec

(** The codec of a JSON schema, linked to its OCaml definiton. *)
val any_schema : (Json_schema.schema, value) codec

(** {2 Exporting [codec]s as JSON schemas} ************************************)

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

(** Name a definition so its occurences can be shared in the JSON schema. *)
val def : string -> ('t, 'k) codec -> ('t, 'k) codec
