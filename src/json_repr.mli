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

(** {2 In memory JSON document representation} *) (****************************)

(** A JSON value compatible with {!Ezjsonm.value}.
    This is the main type used by this library, unlike {!yojson},
    which is provided only for compatibility. *)
type value =
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

(** {2 Paths in JSON documents} *) (*******************************************)

(** An abstract type for paths into a JSON document.
    A sequence of sub-tree selectors to descend into a JSON tree. *)
type path = path_item list

(** A JSON sub-tree selector.
    Indendent from any concrete format (JSON pointer, JSON path, etc.)
    The semantics depends on the use (selection, insertion, etc.) *)
and path_item =
  [ `Field of string
  (** A field in an object. *)
  | `Index of int
  (** An index in an array. *)
  | `Star
  (** Any / every field or index. *)
  | `Next
  (** The next element after an array. *) ]

(** Pretty prints a path in JSON pointer format (RFC6901).  May throw
    {!Unsupported_path_item}. Use [~wildcards:false] to deactivate the
    support of wildcard path items, which may lead to
    {!Unsupported_path_item}. *)
val print_path_as_json_pointer : ?wildcards: bool -> Format.formatter -> path -> unit

(** Pretty prints a path in JSON path format. Use [~wildcards:false] to
    deactivate the support of wildcard path items, which may lead to
    {!Unsupported_path_item}. *)
val print_path_as_json_path : ?wildcards: bool -> Format.formatter -> path -> unit

(** Pretty prints a path in JSON pointer format into a fresh string.
    May throw {!Unsupported_path_item}. Use [~wildcards:false] to
    deactivate the support of wildcard path items, which may lead to
    {!Unsupported_path_item}. *)
val json_pointer_of_path : ?wildcards: bool -> path -> string

(** Parses a path from a string in JSON pointer format.  May throw
    {!Illegal_pointer_notation}. The string is expected to be ASCII
    compatible, including UTF-8. Use [~wildcards:false] to deactivate
    the support of wildcard path items, which may lead to
    {!Unsupported_path_item}. *)
val path_of_json_pointer : ?wildcards: bool -> string -> path

(** {2 Querying JSON documents} *) (*******************************************)

(** Extracts the value located at a given path. If multiple locations
    satisfy the path (in presence of wildcard path items), the chosen
    one is unspecified. May throw [Not_found]. *)
val query : path -> [< value ] -> value

(** Extracts the values located at a given path (may be more than one
    in presence of wildcard path items). The order is unspecified. *)
val query_all : path -> [< value ] -> value list

(** Insert a value at a given path. If multiple locations satisfy the
    path (in presence of wildcard path items), the chosen one is
    unspecified. Will create parent objects or arrays if needed, for
    instance inserting [3] at [/a/b/c] in [{}] will result in
    [{"a":{"b":{"c":3}}}]. Inserting in an array at an index bigger
    than the previous size will expand the array, filling potential
    missing cells with [`Null]. Inserting in an array at [`Index n]
    where [n] is negative inserts from the last element of the
    array. If a value is inserted at a location where there is already
    one, both are merged as if with {!merge}. May throw
    {!Cannot_merge} if the path is incompatible with the original
    object (such as inserting in a field of something which is not an
    object) or if the value is to be merged with an incompatible
    existing value. *)
val insert : path -> [< value ] -> [< value ] -> value

(** Same as {!insert}, except that if the path leads to a pre-existing
    value, it is replaced with the new one instead of being merged. *)
val replace : path -> [< value ] -> [< value ] -> value

(** Merges two compatible JSON values. Merges [`Null] with any JSON
    value. Merges two deeply equal values together. Merges two objects
    by merging their common fields and adding all the others. Merges
    two arrays by merging their common cells pairwise and adding the
    remaining ones if one array is bigger than the other. May throw
    {!Cannot_merge}. *)
val merge : [< value ] -> [< value ] -> value

(** {2 Errors} *) (************************************************************)

(** When two incompatible objects are unsuccessfully merged. Comes
    with the path to the first incompatibility encountered.*)
exception Cannot_merge of path

(** An path litteral could not be parsed.
    Comes with the original string, the position and an explanation. *)
exception Illegal_pointer_notation of string * int * string

(** An operation was given a path containing an unsupported construct.
    Comes with an explanation as its second argument. *)
exception Unsupported_path_item of path_item * string

(** Produces a human readable version of an error. *)
val print_error
  : ?print_unknown: (Format.formatter -> exn -> unit) ->
  Format.formatter -> exn -> unit

(** {2 YoJSON Compatibility} *) (**********************************************)

(** A JSON value compatible with {!Yojson.Safe.json}.
    Provided only for compatibility.
    See converters *)
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

(** Conversion helper. *)
val from_yojson : [< yojson ] -> [> value ]

(** Conversion helper. *)
val to_yojson : [< value] -> [> yojson ]
