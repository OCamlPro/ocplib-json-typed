(** Representations of JSON documents *)

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

(** {2 Abstraction over JSON representations} *) (*****************************)

(** The internal format used by the library. A common format to view
    JSON structures from different representations. It only shows the
    head of structures, hiding the contents of fields, so that the
    conversion from another format or a stream can be done lazily. *)
type 'a view =
  [ `O of (string * 'a) list
    (** An associative table (object). *)
  | `A of 'a list
    (** An (integer indexed) array. *)
  | `Bool of bool
    (** A JS boolean [true] or [false]. *)
  | `Float of float
    (** A floating point number (double precision). *)
  | `String of string
    (** An UTF-8 encoded string. *)
  | `Null
    (** The [null] constant. *) ]

(** A view over a given implementation. *)
module type Repr = sig

  (** The implementation type. *)
  type value

  (** View a value in the common format. *)
  val view : value -> value view

  (** Builds a value from a view *)
  val repr : value view -> value

end

(** Convert a JSON value from one representation to another. *)
val convert :
  (module Repr with type value = 'tf) ->
  (module Repr with type value = 'tt) ->
  'tf -> 'tt

(** {2 Third party in-memory JSON document representations} *) (****************)

(** A JSON value compatible with {!Ezjsonm.value}. *)
type ezjsonm =
  [ `O of (string * ezjsonm) list
  (** An associative table (object). *)
  | `A of ezjsonm list
  (** An (integer indexed) array. *)
  | `Bool of bool
  (** A JS boolean [true] or [false]. *)
  | `Float of float
  (** A floating point number (double precision). *)
  | `String of string
  (** An UTF-8 encoded string. *)
  | `Null
  (** The [null] constant. *) ]

(** A view over the {!ezjsonm} representation.*)
module Ezjsonm : Repr with type value = ezjsonm

(** A JSON value compatible with {!Yojson.Safe.json}. *)
type yojson =
  [ `Bool of bool
  (** A JS boolean [true] of [false]. *)
  | `Assoc of (string * yojson) list
  (** JSON object. *)
  | `Float of float
  (** A floating point number (double precision). *)
  | `Int of int
  (** A number without decimal point or exponent. *)
  | `Intlit of string
  (** A number without decimal point or exponent, preserved as string. *)
  | `List of yojson list
  (** A JS array. *)
  | `Null
  (** The [null] constant. *)
  | `String of string
  (** An UTF-8 encoded string. *)
  | `Tuple of yojson list
  (** A tuple (non-standard). Syntax: ("abc", 123). *)
  | `Variant of string * yojson option
    (** A variant (non-standard). Syntax: <"Foo"> or <"Bar": 123>. *) ]

(** A view over the {!yojson} representation.*)
module Yojson : Repr with type value = yojson

(** A intermediate representation for BSON, a binary encoding for JSON.

    Decoding and encoding is (optionally) done as lazily as possible.
    First, the {!view} function is able to unfold only one
    level and not the whole structure. Also, serialized versions are
    cached, so that later serializations of the same object are faster.

    Notes:

    1. Only JSON compatible BSON documents are supported.
       BSON extensions are not supported (int32, int64, timestamp, etc.).
    2. Arrays in BSON are stored inefficiently.
       Prefer another binary format if you manipulate lots of arrays.
    3. We differ from BSON to allow toplevel immediates.
       For this, we produce a document with only one byte indicating
       the kind of immediate followed by the immediate.
       The byte is [0x80 lor (the corresponding BSON field kind)].
    4. We differ from BSON to allow unambiguous toplevel arrays.
       As with (3), the subdocument to be decoded as an array is
       preceded with a 0x84.

    Use the [conforming] flag to deactivates the extension from notes (3)
    and (4). In this case, the toplevel value must be an object. *)
type bson

(** A view over the {!bson} representation.*)
module Bson : Repr with type value = bson

(** Serializes the intermediate BSON representation to actual BSON.

    By default, [conforming] is [false], so that any value can be serialized,
    including immediates (see {!type:bson}).

    By default, [cache] is [true], so a future serialization of the
    same data will be faster. The resulting bytes are stored in the
    value. You may want to turn this off if these values have a long
    lifespan, and that you care more about memory consumption than
    serialization speed.

    Will raise [Invalid_argument "Json_repr.bson_to_bytes"] when
    [conforming] and trying to serialize a toplevel array or immediate. *)
val bson_to_bytes :
  ?cache: bool -> ?conforming: bool ->
  bson -> bytes

(** Bson decoding error, with a message, the BSON and an offset. *)
exception Bson_decoding_error of string * bytes * int

(** Creates a lazily unfolded representation for some BSON.
    Because of the mutability of [bytes] and this laziness,
    set the copy parameter to [true] if you are not sure that the
    [bytes] will not be mutated in the future.

    By default, [conforming] is [false], so that any value can be serialized,
    including immediates (see {!type:bson}).

    By default, [cache] is [true], so a future serialization of the
    same data will be faster. The input bytes are stored in the
    value. You may want to turn this off if these values have a long
    lifespan, and that you care more about memory consumption than
    serialization speed.

    By default, [laziness] is [true]. If the data is a serialized
    object, it means that only the field names are read, the field
    values are eluded, and will be deserialized on demand when calling
    {!Bson.view}. This implies that {!Bson_decoding_error} may be
    raised later. If set to [false], the whole structure is decoded
    upfront, so any decoding error will happen at this point. This may
    be preferable mostly when reading from untusted sources.

    May raise {!Bson_decoding_error}. *)
val bytes_to_bson :
  ?laziness: bool -> ?cache: bool -> ?conforming: bool ->
  copy: bool -> bytes -> bson

(** {2 Representation-agnostic JSON format} *) (********************************)

(** A meta-representation for JSON values that can unify values of
    different representations by boxing them with their corresponding
    {!Repr} modules. *)
type any = private Value_with_repr: (module Repr with type value = 'a) * 'a -> any

(** Converts a boxed value from its intrinsic representation to the
    one of the given {!Repr} module. Optimized if the internal
    representation of the value actually is the requested one. *)
val any_to_repr : (module Repr with type value = 'a) -> any -> 'a

(** Boxes a value with a compatible {!Repr} module. *)
val repr_to_any : (module Repr with type value = 'a) -> 'a -> any

(** {2 Predefined converters for {!ezjsonm} *) (********************************)

(** Conversion helper. *)
val from_yojson : [< yojson ] -> [> ezjsonm ]

(** Conversion helper. *)
val to_yojson : [< ezjsonm] -> [> yojson ]

(** Converts a boxed value from its representation to {!ezjsonm}. *)
val from_any : any -> [> ezjsonm ]

(** Boxes as {!ezjsonm} value. *)
val to_any : [< ezjsonm] -> any
