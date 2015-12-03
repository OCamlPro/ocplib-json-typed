(* Abstract representation of JSON schemas. *)

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

(* TODO: validator *)

open Json_repr

(* The currently handled version *)
let version = "http://json-schema.org/draft-04/schema#"

(*-- types and errors --------------------------------------------------------*)

(* The root of a schema with the named definitions,
   a precomputed ID-element map and a cache for external documents. *)
type schema =
  { root : element ;
    source : Uri.t (* whose fragment should be empty *) ;
    definitions : (path * element) list ;
    ids : (string * element) list ;
    world : schema list }

and element =
  { title : string option ;
    description : string option ;
    default : value option ;
    enum : value list option ;
    kind : element_kind ;
    format : string option ;
    id : string option }

and element_kind =
  | Object of object_specs
  | Array of element list * array_specs
  | Monomorphic_array of element * array_specs
  | Combine of combinator * element list
  | Def_ref of path
  | Id_ref of string
  | Ext_ref of Uri.t
  | String of string_specs
  | Integer | Number | Boolean | Null | Any
  | Dummy

and combinator =
  | Any_of | One_of | All_of | Not

and array_specs =
  { min_items : int ;
    max_items : int option ;
    unique_items : bool ;
    additional_items : element option }

and object_specs =
  { properties : (string * element * bool * Json_repr.value option) list ;
    pattern_properties : (string * element) list ;
    additional_properties : element option ;
    min_properties : int ;
    max_properties : int option ;
    schema_dependencies : (string * element) list ;
    property_dependencies : (string * string list) list }

and string_specs =
  { pattern : string option ;
    min_length : int ;
    max_length : int option }

exception Duplicate_definition of path
exception Bad_reference of string
exception Cannot_parse of path * exn
exception Dangling_reference of Uri.t
exception Unexpected of string * string

(* box an element kind without any optional field *)
let element kind =
  { title = None ; description = None ; default = None ; kind ;
    format = None ; enum = None ; id = None }

(*-- internal definition table handling --------------------------------------*)

let find_definition name defs =
  List.assoc name defs

let definition_exists name defs =
  List.mem_assoc name defs

let insert_definition name elt defs =
  let rec insert = function
    | [] ->
      [ (name, elt) ]
    | (defname, _) as def :: rem when defname <> name ->
      def :: insert rem
    | (_, { kind = Dummy }) :: rem ->
      (name, elt) :: rem
    | (_, defelt) :: rem ->
      if elt <> defelt then raise (Duplicate_definition name) ;
      (name, elt) :: rem in
  insert defs

(*-- printer -----------------------------------------------------------------*)

let to_json schema =
  (* functional JSON building combinators *)
  let (@) (`O l1) (`O  l2) = `O (l1 @ l2) in
  let set_always f v = `O [ f, v ] in
  let set_if_some f v cb = match v with None -> `O [] | Some v -> `O [ f, cb v ] in
  let set_if_cons f v cb = match v with [] -> `O [] | v -> `O [ f, cb v ] in
  let set_if_neq f v v' cb = if v <> v' then `O [ f, cb v ] else `O [] in
  (* recursive encoder *)
  let rec format_element
    : 'a. element -> ([> `O of (string * value) list ] as 'a)
    = fun { title ; description ; default ; enum ; kind ; format } ->
      set_if_some "title" title (fun s -> `String s) @
      set_if_some "description" description (fun s -> `String s) @
      begin match kind with
        | Object specs ->
          let required = List.fold_left
              (fun r (n, _, p, _) -> if p then `String n :: r else r)
              [] specs.properties in
          let properties =
            List.map
              (fun (n, elt, _, _) -> n, format_element elt)
              specs.properties in
          set_always "type" (`String "object") @
          set_always "properties" (`O properties) @
          set_if_cons "required" required (fun l -> `A l) @
          set_if_cons "patternProperties" specs.pattern_properties
            (fun fs -> `O (List.map (fun (n, elt) -> n, format_element elt) fs)) @
          set_if_neq "additionalProperties" specs.additional_properties (Some (element Any))
            (function
              | None -> `Bool false
              | Some elt -> format_element elt) @
          set_if_neq "minProperties" specs.min_properties 0
            (fun i -> `Float (float i)) @
          set_if_some "maxProperties" specs.max_properties
            (fun i -> `Float (float i)) @
          set_if_cons "schemaDependencies" specs.schema_dependencies
            (fun fs -> `O (List.map (fun (n, elt) -> n, format_element elt) fs)) @
          set_if_cons "propertyDependencies" specs.property_dependencies
            (fun fs ->
               let property_dependencies =
                 List.map (fun (n, ls) -> n, `A (List.map (fun s -> `String s) ls)) fs in
               `O property_dependencies)
        | Array (elts, specs) ->
          set_always "type" (`String "array") @
          set_always "items" (`A (List.map format_element elts)) @
          set_if_neq "minItems" specs.min_items 0 (fun i -> `Float (float i)) @
          set_if_some "maxItems" specs.max_items (fun i -> `Float (float i)) @
          set_if_neq "uniqueItems" specs.unique_items false (fun b -> `Bool b) @
          set_if_neq "additionalItems"
            specs.additional_items (Some (element Any))
            (function
              | None -> `Bool false
              | Some elt -> format_element elt)
        | Monomorphic_array (elt, {min_items ; max_items ; unique_items }) ->
          set_always "type" (`String "array") @
          set_always "items" (format_element elt) @
          set_if_neq "minItems"
            min_items 0
            (fun i -> `Float (float i)) @
          set_if_some "maxItems"
            max_items
            (fun i -> `Float (float i)) @
          set_if_neq "uniqueItems"
            unique_items false
            (fun b -> `Bool b)
        | Combine (c, elts) ->
          let combinator = function
            | Any_of -> "anyOf"
            | One_of -> "oneOf"
            | All_of -> "allOf"
            | Not -> "not" in
          set_always (combinator c) (`A (List.map format_element elts))
        | Def_ref path ->
          set_always "$ref" (`String ("#" ^ (json_pointer_of_path path)))
        | Id_ref name ->
          set_always "$ref" (`String ("#" ^ name))
        | Ext_ref uri ->
          set_always "$ref" (`String (Uri.to_string uri))
        | Integer ->
          set_always "type" (`String "integer")
        | Number ->
          set_always "type" (`String "number")
        | String { pattern ; min_length ; max_length } ->
          set_always "type" (`String "string") @
          set_if_neq "minLength" min_length 0 (fun i -> `Float (float i)) @
          set_if_some "maxLength" max_length (fun i -> `Float (float i)) @
          set_if_some "pattern" pattern (fun s -> `String s)
        | Boolean ->
          `O [ "type", `String "boolean" ]
        | Null ->
          `O [ "type", `String "null" ]
        | Dummy ->
          invalid_arg "Json_schema.to_json: remaining dummy element"
        | Any -> `O [] end @
      set_if_some "default" default  (fun j -> j) @
      set_if_some "enum" enum (fun js -> `A js) @
      set_if_some "format" format (fun s -> `String s) in
  let json =
    List.fold_left
      (fun acc (n, elt) -> insert n (format_element elt) acc)
      (set_always "$schema" (`String version) @ format_element schema.root)
      schema.definitions in
  match json with `O _ as json -> json | _ -> assert false (* absurd *)

let rec print_error ?print_unknown ppf = function
  | Cannot_parse (path, exn) ->
    Format.fprintf ppf
      "@[<v 2>Schema parse error:@,At %a@,%a@]"
      (Json_repr.print_path_as_json_path ~wildcards:true) path
      (print_error ?print_unknown) exn
  | Dangling_reference uri ->
    Format.fprintf ppf
      "Dangling reference %s" (Uri.to_string uri)
  | Bad_reference str ->
    Format.fprintf ppf
      "Illegal reference notation %s" str
  | Unexpected (unex, ex) ->
    Format.fprintf ppf
      "Unexpected %s instead of %s" unex ex
  | exn ->
    Json_repr.print_error ?print_unknown ppf exn

let unexpected kind expected =
  let kind =match kind with
    | `O [] -> "empty object"
    | `A [] -> "empty array"
    | `O _ -> "object"
    | `A _ -> "array"
    | `Null -> "null"
    | `String "" -> "empty string"
    | `String _ -> "string"
    | `Float _ -> "number"
    | `Bool _ -> "boolean" in
  Cannot_parse ([], Unexpected (kind, expected))

(*-- parser ------------------------------------------------------------------*)

let at_path p = function Cannot_parse (l, err) -> Cannot_parse (p @ l, err) | exn -> exn
let at_field n = at_path [ `Field n ]
let at_index i = at_path [ `Index i ]

let of_json json =
  (* parser combinators *)
  let opt_field obj n = match obj with
    | `O ls -> (try Some (List.assoc n ls) with Not_found -> None)
    | _ -> None in
  let opt_string_field obj n = match opt_field obj n with
    | Some (`String s) -> Some s
    | Some k -> raise (at_field n @@ unexpected k "string")
    | None -> None in
  let opt_bool_field def obj n = match opt_field obj n with
    | Some (`Bool b) -> b
    | Some k -> raise (at_field n @@ unexpected k "bool")
    | None -> def in
  let opt_int_field obj n = match opt_field obj n with
    | Some (`Float f) when floor f = f -> Some (int_of_float f)
    | Some k -> raise (at_field n @@ unexpected k "integer")
    | None -> None in
  let opt_array_field obj n = match opt_field obj n with
    | Some (`A s) -> Some s
    | Some k -> raise (at_field n @@ unexpected k "array")
    | None -> None in
  let opt_uri_field obj n = match opt_string_field obj n with
    | None -> None
    | Some uri ->
      match Uri.canonicalize (Uri.of_string uri) with
      | exception _ -> raise (Cannot_parse ([], Bad_reference (uri ^ " is not a valid URI")))
      | uri -> Some uri in
  (* local resolution of definitions *)
  let schema_source = match opt_uri_field json "id" with
    | Some uri -> Uri.with_fragment uri None
    | None -> Uri.empty in
  let collected_definitions = ref [] in
  let collected_id_defs = ref [] in
  let collected_id_refs = ref [] in
  let rec collect_definition : Uri.t -> element_kind = fun uri ->
    match Uri.host uri, Uri.fragment uri with
    | Some _ (* Actually means: any of host, user or port is defined. *), _ ->
      Ext_ref uri
    | None, None ->
      raise (Cannot_parse ([], Bad_reference (Uri.to_string uri ^ " has no fragment")))
    | None, Some fragment when not (String.contains fragment '/') ->
      collected_id_refs := fragment :: !collected_id_refs ;
      Id_ref fragment
    | None, Some fragment ->
      let path =
        try path_of_json_pointer ~wildcards:false fragment
        with err -> raise (Cannot_parse ([], err)) in
      try
        let raw = query path json in
        if not (definition_exists path !collected_definitions) then begin
          (* dummy insertion so we don't recurse and we support cycles *)
          collected_definitions := insert_definition path (element Dummy) !collected_definitions ;
          let elt = try parse_element schema_source raw
            with err -> raise (at_path path err) in
          (* actual insertion *)
          collected_definitions := insert_definition path elt !collected_definitions
        end ;
        Def_ref path
      with Not_found -> raise (Cannot_parse ([], Dangling_reference uri))
  (* recursive parser *)
  and parse_element : Uri.t -> value -> element = fun source json ->
    let id = opt_uri_field json "id" in
    let id, source = match id with
      | None -> None, source
      | Some uri ->
        let uri = Uri.canonicalize (Uri.resolve "http" source uri) in
        Uri.fragment uri, Uri.with_fragment uri None in
    (* We don't support inlined schemas, so we just drop elements with
       external sources and replace them with external references. *)
    if source <> schema_source then
      element (Ext_ref (Uri.with_fragment source id))
    else
      let id = match id with
        | None -> None
        | Some id when String.contains id '/' ->
          raise (at_field "id" @@ Cannot_parse ([], Bad_reference (id ^ " is not a valid ID")))
        | Some id -> Some id in
      (* We parse the various element syntaxes and combine them afterwards. *)
      (* 1. An element with a known type field and associated fields. *)
      let as_kind =
        match opt_field json "type" with
        | Some (`String name) ->
          Some (element (parse_element_kind source json name))
        | Some (`A [] as k) ->
          raise (at_field "type" @@ unexpected k "type, type array or operator")
        | Some (`A l) ->
          let rec items i acc = function
            | [] ->
              let kind = Combine (Any_of, List.rev acc) in
              Some (element kind)
            | `String name :: tl ->
              let kind = parse_element_kind source json name in
              let case = element kind in
              items (succ i) (case :: acc) tl
            | k :: tl ->
              raise (at_field "type" @@ at_index i @@ unexpected k "type")
          in items 0 [] l
        | Some k ->
          raise (at_field "type" @@ unexpected k "type, type array or operator")
        | None -> None in
      (* 2. A reference *)
      let as_ref =
        match opt_uri_field json "$ref" with
        | Some uri ->
          let path = collect_definition uri in
          Some (element path)
        | None -> None in
      (* 3. Combined schemas *)
      let rec as_nary name combinator others =
        let build = function
          | [] -> None (* not found and no auxiliary case *)
          | [ case ] -> Some case  (* one case -> simplify *)
          | cases -> (* several cases build the combination node with empty options *)
            let kind = Combine (combinator, cases) in
            Some (element kind) in
        match opt_field json name with
        | Some (`A (_ :: _ as cases)) (* list of schemas *) ->
          let rec items i acc = function
            | elt :: tl ->
              let elt = try parse_element source elt
                with err -> raise (at_field name @@ at_index i @@ err) in
              items (succ i) (elt :: acc) tl
            | [] ->
              build (others @ List.rev acc)
          in items 0 [] cases
        | None -> build others
        | Some k -> raise (at_field name @@ unexpected k "a list of elements") in
      (* 4. Negated schema *)
      let rec as_not =
        match opt_field json "not" with
        | None -> None
        | Some elt ->
          let elt = try parse_element source elt
            with err -> raise (at_field "not" err) in
          let kind = Combine (Not, [ elt ]) in
          Some (element kind) in
      (* parse optional fields *)
      let title = opt_string_field json "title" in
      let description = opt_string_field json "description" in
      let default = opt_field json "default" in
      let enum = opt_array_field json "enum" in
      let format = opt_string_field json "format" in (* TODO: check format ? *)
      (* combine all specifications under a big conjunction *)
      let as_one_of = as_nary "oneOf" One_of [] in
      let as_any_of = as_nary "anyOf" Any_of [] in
      let all = [ as_kind ; as_ref ; as_not ; as_one_of ; as_any_of ] in
      let cases = List.flatten (List.map (function None -> [] | Some e -> [ e ]) all) in
      let kind = match as_nary "allOf" All_of cases with
        | None -> Any (* no type, ref or logical combination found *)
        | Some { kind } -> kind in
      (* add optional fields *)
      { title ; description ; default ; format ; kind ; enum ; id }
  and parse_element_kind
    : type a. Uri.t -> value -> string -> element_kind
    = fun source json name ->
      match name with
      | "integer" -> Integer
      | "number" -> Number
      | "boolean" -> Boolean
      | "null" -> Null
      | "string" ->
        let specs =
          let pattern = opt_string_field json "pattern" in
          let min_length = opt_int_field json "minLength" in
          let max_length = opt_int_field json "maxLength" in
          let min_length = match min_length with None -> 0 | Some l -> l in
          { pattern ; min_length ; max_length } in
        String specs
      | "array" ->
        let specs =
          let unique_items = opt_bool_field false json "uniqueItems" in
          let min_items = opt_int_field json "minItems" in
          let max_items = opt_int_field json "maxItems" in
          let min_items = match min_items with None -> 0 | Some l -> l in
          match opt_field json "additionalItems" with
          | None | Some (`Bool true) ->
            { min_items ; max_items ; unique_items ; additional_items = Some (element Any) }
          | Some (`Bool false) ->
            { min_items ; max_items ; unique_items ; additional_items = None }
          | Some elt ->
            let elt = try parse_element source elt
              with err -> raise (at_field "additionalItems" err) in
            { min_items ; max_items ; unique_items ; additional_items = Some elt } in
        begin match opt_field json "items" with
          | Some (`A elts) ->
            let rec elements i acc = function
              | [] ->
                Array (List.rev acc, specs)
              | elt :: tl ->
                let elt = try parse_element source elt
                  with err -> raise (at_field "items" @@ at_index i err) in
                elements (succ i) (elt :: acc) tl
            in elements 0 [] elts
          | Some elt ->
            let elt = try parse_element source elt
              with err -> raise (at_field "items" err) in
            Monomorphic_array (elt, specs)
          | None ->
            Monomorphic_array (element Any, specs) end
      | "object" ->
        let required =
          match opt_array_field json "required" with
          | None ->  []
          | Some l ->
            let rec items i acc = function
              | `String s :: tl -> items (succ i) (s :: acc) tl
              | [] -> List.rev acc
              | k :: _ -> raise (at_field "required" @@ at_index i @@ unexpected k "string")
            in items 0 [] l in
        let properties =
          match opt_field json "properties" with
          | Some (`O props) ->
            let rec items acc = function
              | [] -> List.rev acc
              | (n, elt) :: tl ->
                let elt = try parse_element source elt
                  with err -> raise (at_field "properties" @@ at_field n @@ err) in
                let req = List.mem n required in
                items ((n, elt, req, None) :: acc) tl (* XXX: fixme *)
            in items [] props
          | None -> []
          | Some k -> raise (at_field "properties" @@ unexpected k "object") in
        let additional_properties =
          match opt_field json "additionalProperties" with
          | Some (`Bool false) -> None
          | None | Some (`Bool true) -> Some (element Any)
          | Some elt ->
            let elt = try parse_element source elt
              with err -> raise (at_field "additionalProperties" err) in
            Some elt in
        let property_dependencies =
          match opt_field json "propertyDependencies" with
          | None -> []
          | Some (`O l) ->
            let rec sets sacc = function
              | (n, `A l) :: tl ->
                let rec strings j acc = function
                  | [] -> sets ((n, List.rev acc) :: sacc) tl
                  | `String s :: tl -> strings (succ j) (s :: acc) tl
                  | k :: _ ->
                    raise (at_field "propertyDependencies" @@
                           at_field n @@
                           at_index j @@
                           unexpected k "string")
                in strings 0 [] l
              | (n, k) :: tl ->
                raise (at_field "propertyDependencies" @@
                       at_field n @@
                       unexpected k "string array")
              | [] -> List.rev sacc
            in sets [] l
          | Some k ->
            raise (at_field "propertyDependencies" @@
                   unexpected k "object") in
        let parse_element_assoc field =
          match opt_field json field with
          | None -> []
          | Some (`O props) ->
            let rec items acc = function
              | [] -> List.rev acc
              | (n, elt) :: tl ->
                let elt = try parse_element source elt
                  with err -> raise (at_field field @@
                                     at_field n err) in
                items ((n, elt) :: acc) tl
            in items [] props
          | Some k -> raise (at_field field @@ unexpected k "object") in
        let pattern_properties = parse_element_assoc "patternProperties" in
        let schema_dependencies = parse_element_assoc "schemaDependencies" in
        let min_properties =
          match opt_int_field json "minProperties" with
          | None -> 0
          | Some l -> l in
        let max_properties = opt_int_field json "maxProperties" in
        Object { properties ; pattern_properties ;
                 additional_properties ;
                 min_properties ; max_properties ;
                 schema_dependencies ; property_dependencies }
      | n -> raise (Cannot_parse ([], Unexpected (n, "a known type"))) in
  (* parse recursively from the root *)
  let root = parse_element Uri.empty (json :> value) in
  (* force the addition of everything inside /definitions *)
  (match query [ `Field "definitions" ] (json :> value) with
   | `O all ->
     let all = List.map (fun (n, _) -> Uri.of_string ("#/definitions/" ^ n)) all in
     List.iter (fun uri -> collect_definition uri |> ignore) all
   | _ -> ()
   | exception  Not_found -> ()) ;
  (* check the domain of IDs *)
  List.iter
    (fun id ->
       if not (List.mem_assoc id !collected_id_defs) then
         raise (Cannot_parse ([], Dangling_reference (Uri.(with_fragment empty (Some id))))))
    !collected_id_refs ;
  let ids = !collected_id_defs in
  let source = schema_source in
  let world = [] in
  let definitions = !collected_definitions in
  { root ; definitions ; source ; ids ; world }

(*-- creation and update -----------------------------------------------------*)

(* Checks that all local refs and ids are defined *)
let check_definitions root definitions =
  let collected_id_defs = ref [] in
  let collected_id_refs = ref [] in
  let rec check ({ kind ; id } as elt) =
    begin match id with
    | None -> ()
    | Some id -> collected_id_defs := (id, elt) :: !collected_id_defs end ;
    begin match kind with
    | Object { properties ; pattern_properties ;
               additional_properties ; schema_dependencies } ->
      List.iter (fun (_, e, _, _) -> check e) properties ;
      List.iter (fun (_, e) -> check e) pattern_properties ;
      List.iter (fun (_, e) -> check e) schema_dependencies ;
      (match additional_properties with Some e -> check e | None -> ())
    | Array (es, { additional_items }) ->
      List.iter check es ;
      (match additional_items with Some e -> check e | None -> ())
    | Monomorphic_array (e, { additional_items }) ->
      check e ;
      (match additional_items with Some e -> check e | None -> ())
    | Combine (_, es) ->
      List.iter check es
    | Def_ref path ->
      if not (definition_exists path definitions) then
        let path = json_pointer_of_path path in
        raise (Dangling_reference (Uri.(with_fragment empty) (Some path)))
    | Id_ref id ->
      collected_id_refs := id :: !collected_id_refs ;
    | Ext_ref _ | String _ | Integer | Number | Boolean | Null | Any | Dummy -> ()
    end in
  (* check the root and definitions *)
  check root ;
  List.iter (fun (_, root) -> check root) definitions ;
  (* check the domain of IDs *)
  List.iter
    (fun id ->
       if not (List.mem_assoc id !collected_id_defs) then
         raise (Dangling_reference (Uri.(with_fragment empty (Some id)))))
    !collected_id_refs ;
  !collected_id_defs

let create root =
  let ids = check_definitions root [] in
  { root ; definitions = [] ; world = [] ; ids ; source = Uri.empty }

let root { root } =
  root

let update root sch =
  let ids = check_definitions sch.root sch.definitions in
  { sch with root ; ids }

let any =
  create (element Any)

let self =
  { root = element (Ext_ref (Uri.of_string version)) ;
    definitions = [] ; ids = [] ; world = [] ; source = Uri.empty }

(* remove unused definitions from the schema *)
let simplify schema =
  let res = ref [] (* collected definitions *) in
  let rec collect { kind } = match kind with
    | Object { properties ; pattern_properties ;
               additional_properties ; schema_dependencies } ->
      List.iter (fun (_, e, _, _) -> collect e) properties ;
      List.iter (fun (_, e) -> collect e) pattern_properties ;
      List.iter (fun (_, e) -> collect e) schema_dependencies ;
      (match additional_properties with Some e -> collect e | None -> ())
    | Array (es, { additional_items }) ->
      List.iter collect es ;
      (match additional_items with Some e -> collect e | None -> ())
    | Monomorphic_array (e, { additional_items }) ->
      collect e ;
      (match additional_items with Some e -> collect e | None -> ())
    | Combine (_, es) ->
      List.iter collect es
    | Def_ref path ->
      let def = find_definition path schema.definitions in
      res := insert_definition path def !res
    | Ext_ref _ | Id_ref _ | String _ | Integer | Number | Boolean | Null | Any | Dummy -> ()
  in
  collect schema.root ;
  { schema with definitions = !res }

let definition_path_of_name name =
  path_of_json_pointer ~wildcards:false @@
  match String.get name 0 with
    | exception _ -> raise (Bad_reference name)
    | '/' -> name
    | _ -> "/definitions/" ^ name

let find_definition name schema =
  let path = definition_path_of_name name in
  find_definition path schema.definitions

let definition_exists name schema =
  let path = definition_path_of_name name in
  definition_exists path schema.definitions

let add_definition name elt schema =
  let path = definition_path_of_name name in
  (* check inside def *)
  let definitions = insert_definition path elt schema.definitions in
  { schema with definitions }, element (Def_ref path)

let merge_definitions (sa, sb) =
  let rec sorted_merge = function
    | ((na, da) as a) :: ((nb, db) as b) :: tl ->
      if na = nb then
        if da.kind = Dummy || db.kind = Dummy || da = db then
          (na, da) :: sorted_merge tl
        else
          raise (Duplicate_definition na)
      else
        a :: sorted_merge (b :: tl)
    | [] | [ _ ] as rem -> rem
  in
  let definitions =
    sorted_merge (List.sort compare (sa.definitions @ sb.definitions)) in
  { sa with definitions }, { sb with definitions }

let combine op schemas =
  let rec combine sacc eacc = function
    | [] -> update (element (Combine (op, eacc))) sacc
    | s :: ss ->
      let sacc, s = merge_definitions (sacc, s) in
      combine sacc (s.root :: eacc) ss
  in combine any [] schemas

(*-- default specs -----------------------------------------------------------*)

let array_specs =
  { min_items = 0 ;
    max_items = None ;
    unique_items = false ;
    additional_items = None }
let object_specs =
  { properties = [] ;
    pattern_properties = [] ;
    additional_properties = Some (element Any) ;
    min_properties = 0 ;
    max_properties = None ;
    schema_dependencies = [] ;
    property_dependencies = [] }
let string_specs =
  { pattern = None ;
    min_length = 0 ;
    max_length = None }
