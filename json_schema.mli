(** Abstract representation of JSON schemas as of version
    [http://json-schema.org/draft-04/schema#] *)

(** {2 In memory JSON document representation} ********************************)

(** A non toplevel JSON value, structure or immediate. *)
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

(** {2 Abstract representation of schemas} ************************************)

(** The root of the JSON schema *)
type schema = private
  { root : element ;
    (** The toplevel node *)
    definitions : definitions list
    (** A list of named abbreviations *) }

(** The hierarchy of named definitions *)
and definitions = private
  | Group of string * definitions list
  (** A named subtree *)
  | Definition of string * element
  (** A named element definition *)

(** A node in the schema, embeds all type-agnostic specs *)
and element =
  { title : string option ;
    (** An optional short description *)
    description : string option ;
    (** An optional long description *)
    default : value option ;
    (** A default constant to be substituted in case of a missing value *)
    enum : value list option ;
    (** A valid value must equal one of these constants *)
    kind : element_kind ;
    (** The type-specific part *)
    format : string option ;
    (** predefined formats such as [date-time], [email], [ipv4], [ipv6], [uri], *) }

(** The type-specific part of schema nodes *)
and element_kind =
  | Object of object_specs
  (** The type of an object *)
  | Array of element list * array_specs
  (** An fixed-length array with the types of its elements (a tuple) *)
  | Monomorphic_array of element * array_specs
  (** A variable-length array with the type of its children *)
  | Combine of combinator * element list
  (** A mix of schemas using logical combinators *)
  | Def of string list
  (** A named ref to an element in the [definitions] table *)
  | Ref of string
  (** An external reference *)
  | String of string_specs (** A string (with optional characteristics) *)
  | Integer (** Any int *)
  | Number (** Any number *)
  | Boolean  (** Any boolean *)
  | Null (** The null value *)
  | Any (** Any JSON value *)
  | Dummy
  (** For building cyclic definitions, a definition bound to a dummy
      will be considered absent for {!insert_definition} but present
      for {!update}. The idea is to insert a dummy definition, build a
      cyclic structure using it for recursion, and finally update the
      definition with the structure. *)

(** Grammar combinators *)
and combinator =
  | Any_of (** Logical OR n-ary combinator *)
  | One_of (** Logical XOR n-ary combinator *)
  | All_of (** Logical AND n-ary combinator *)
  | Not (** Logical NOT unary combinator *)

(** Parameters of the [Array] and [MonomorphicArray] type specifiers *)
and array_specs =
  { min_items : int ;
    (** The minimum number of elements *)
    max_items : int option ;
    (** The maximum number of elements *)
    unique_items : bool ;
    (** Teels if all elements must be different *)
    additional_items : element option ;
    (** The type of additional items, if allowed *) }

(** Parameters of the [Object] type specifier *)
and object_specs =
  { properties : (string * element * bool) list ;
    (** The names and types of properties, with a flag to indicate if
        they are required ([true]) or optional *)
    pattern_properties : (string * element) list ;
    (** Alternative definition of properties, matching field names
        using regexps instead of constant strings *)
    additional_properties : element option ;
    (** The type of additional properties, if allowed *)
    min_properties : int ;
    (** The minimum number of properties *)
    max_properties : int option ;
    (** The maximum number of properties *)
    schema_dependencies : (string * element) list ;
    (** Additional schemas the value must verify if a property is
        present (property, additional schema) *)
    property_dependencies : (string * string list) list
    (** Additional properties required whenever some property is
        present (property, additional properties) *) }

(** Parameters of the [String] type specifier *)
and string_specs =
  { pattern : string option ;
    (** A regexp the string must conform to *)
    min_length : int ;
    (** The minimum string length *)
    max_length : int option
    (** The maximum string length *) }

(** {2 Combinators to build schemas and elements} *****************************)

(** Construct a naked element (all optional properties to None)  *)
val element : element_kind -> element

(** Construct a schema from its root, without any definition ; the
    element is checked not to contain any [Def] element *)
val create : element -> schema

(** Update a schema from its root, using the definitions from an
    existing schema ; the element is checked to contain only valid
    [Def] elements ; unused definitions are kept, see {simplify} *)
val update : element -> schema -> schema

(** Describes the implemented schema specification as a schema *)
val self : schema

(** A completely generic schema, without any definition *)
val any : schema

(** Combines several schemas *)
val combine : combinator -> schema list -> schema

(** {2 Named definitions} *****************************************************)

(** Merges the definitions of two schemas if possible and returns the
    updated schemas, so that their elements can be mixed without
    introducing dangling references ; if two different definitions are
    bound to the same path, [Invalid_argument
    "Json_schema.insert_definition"] will be raised *)
val merge_definitions : schema * schema -> schema * schema

(** Remove the definitions that are not present in the schema *)
val simplify : schema -> schema

(** Adds a definition by its path, may raise [Invalid_argument
    "Json_schema.insert_definition"] if this path is invalid or
    already used, returns the modified schema and the [Def] node that
    references this definition to be used in the schema *)
val add_definition : string list -> element -> schema -> schema * element

(** Finds a definition by its path, may raise [Not_found] *)
val find_definition : string list -> schema -> element

(** Tells if a path leads to a definition *)
val definition_exists : string list -> schema -> bool

(** {2 Predefined values} *****************************************************)

(** Default Parameters of the [Array] and [MonomorphicArray] type specifiers *)
val array_specs : array_specs

(** Default parameters of the [Object] type specifier *)
val object_specs : object_specs

(** Default parameters of the [String] type specifier *)
val string_specs : string_specs

(** {2 JSON Serialization} ****************************************************)

(** Formats a JSON schema as its JSON representation *)
val to_json : schema -> value

(** Parse a JSON structure as a JSON schema, if possible. *)
val of_json : value -> schema option
