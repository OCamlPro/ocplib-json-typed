(** Representations of JSON documents *)

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

(** Pretty prints a path in JSON pointer format (RFC6901).
    May throw {!Unsupported_path_item}. *)
val print_path_as_json_pointer : Format.formatter -> path -> unit

(** Pretty prints a path in JSON path format. *)
val print_path_as_json_path : Format.formatter -> path -> unit

(** Pretty prints a path in JSON pointer format into a fresh string.
    May throw {!Unsupported_path_item}. *)
val json_pointer_of_path : path -> string

(** Parses a path from a string in JSON pointer format.
    May throw {!Illegal_pointer_notation!}.
    The string is expected to be ASCII compatible, including UTF-8. *)
val path_of_json_pointer : string -> path

(** {2 Querying JSON documents} ***********************************************)

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

(** {2 Errors} ****************************************************************)

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
