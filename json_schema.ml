(* TODO: JSON pointers (other than #/...) *)
(* TODO: validator *)

type json =
  [ `O of (string * json) list
  | `Bool of bool
  | `Float of float
  | `A of json list
  | `Null
  | `String of string ]

type schema =
  { root : element ;
    definitions : definitions list }

and definitions =
  | Group of string * definitions list
  | Definition of string * element

and element =
  { title : string option ;
    description : string option ;
    default : json option ;
    enum : json list option ;
    kind : element_kind ;
    format : string option }

and element_kind =
  | Object of object_specs
  | Array of element list * array_specs
  | Monomorphic_array of element * array_specs
  | Combine of combinator * element list
  | Def of string list
  | Ref of string
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
  { properties : (string * element * bool) list ;
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

(* box an element kind without any optional field *)
let element kind =
  { title = None ; description = None ; default = None ; kind ;
    format = None ; enum = None }

(* internal definition finding *)
let rec find_definition path tree =
  match tree, path with
  | Definition (n, elt) :: _, [ hd ] when n = hd ->
      elt
  | Group (n, defs) :: _, hd :: tl when n = hd ->
      find_definition tl defs
  | g :: rest, path  ->
      find_definition path rest
  | _, []
  | [], _ -> raise Not_found

(* internal definition existence test *)
let rec definition_exists path tree =
  match tree, path with
  | Definition (n, elt) :: _, [ hd ] when n = hd ->
      true
  | Group (n, defs) :: _, hd :: tl when n = hd ->
      definition_exists tl defs
  | g :: rest, path  ->
      definition_exists path rest
  | _, []
  | [], _ -> false

(* internal definition insertion *)
let rec insert_definition path elt tree =
  match tree, path with
  | _, [] ->
      invalid_arg "Json_schema.insert_definition"
  | Definition (n, prev) :: rest, [ hd ] when n = hd ->
      if prev.kind = Dummy || prev = elt then
        Definition (n, elt) :: rest
      else
        invalid_arg "Json_schema.insert_definition"
  | Definition (n, _) :: _, hd :: _ when n = hd ->
      invalid_arg "Json_schema.insert_definition"
  | Group (n, _) :: _, [ hd ] when n = hd ->
      invalid_arg "Json_schema.insert_definition"
  | Group (n, defs) :: rest, hd :: (_ :: _ as tl) when n = hd ->
      Group (n, insert_definition tl elt defs) :: rest
  | Group (n, _) as g :: rest, hd :: _ :: _ when n = hd ->
      g :: insert_definition path elt rest
  | g :: rest, path  ->
      g :: insert_definition path elt rest
  | [], hd :: (_ :: _ as tl) ->
      [ Group (hd, insert_definition tl elt []) ]
  | [], [ hd ] ->
      [ Definition (hd, elt) ]

(* OCaml definition -> JSON encoding *)
let to_json schema =
  (* functional JSON building combinators *)
  let option_field v f cb (`O fs) =
    match v with
    | Some v -> `O ((f, cb v) :: fs)
    | None -> `O fs in
  let list_field v f cb (`O fs) =
    match v with
    | [] -> `O fs
    | _ -> `O ((f, cb v) :: fs) in
  let neq_field v v' f cb (`O fs) =
    if v <> v' then
      `O ((f, cb v) :: fs)
    else
      `O fs in
  (* recursive encoder *)
  let rec format_element { title ; description ; default ; enum ; kind ; format } =
    (match kind with
     | Object specs ->
         let required = List.fold_left
             (fun r (n, _, p) -> if p then `String n :: r else r)
             [] specs.properties in
         let main_properties =
           `O [ "type", `String "object" ;
                    "properties", `O (List.map
                                            (fun (n, elt, _) -> n, format_element elt)
                                            specs.properties) ] in
         main_properties |>
         list_field required
           "required"
           (fun l -> `A l) |>
         list_field specs.pattern_properties
           "patternProperties"
           (fun fs -> `O (List.map (fun (n, elt) -> n, format_element elt) fs)) |>
         neq_field specs.additional_properties (Some (element Any))
           "additionalProperties"
           (function
             | None -> `Bool false
             | Some elt -> format_element elt) |>
         neq_field specs.min_properties 0
           "minProperties"
           (fun i -> `Float (float i)) |>
         option_field specs.max_properties
           "maxProperties"
           (fun i -> `Float (float i)) |>
         list_field specs.schema_dependencies
           "schemaDependencies"
           (fun fs -> `O (List.map (fun (n, elt) -> n, format_element elt) fs)) |>
         list_field specs.property_dependencies
           "propertyDependencies"
           (fun fs ->
              let property_dependencies =
                List.map (fun (n, ls) -> n, `A (List.map (fun s -> `String s) ls)) fs in
              `O property_dependencies)
     | Array (elts, specs) ->
         let main_properties =
           `O [ "type", `String "array" ;
                    "items", `A (List.map format_element elts) ] in
         main_properties |>
         neq_field specs.min_items 0
           "minItems"
           (fun i -> `Float (float i)) |>
         option_field specs.max_items
           "maxItems"
           (fun i -> `Float (float i)) |>
         neq_field specs.unique_items false
           "uniqueItems"
           (fun b -> `Bool b) |>
         neq_field specs.additional_items (Some (element Any))
           "additionalItems"
           (function
             | None -> `Bool false
             | Some elt -> format_element elt)
     | Monomorphic_array (elt, {min_items ; max_items ; unique_items }) ->
         let main_properties =
           `O [ "type", `String "array" ;
                    "items", format_element elt] in
         main_properties |>
         neq_field min_items 0
           "minItems"
           (fun i -> `Float (float i)) |>
         option_field max_items
           "maxItems"
           (fun i -> `Float (float i)) |>
         neq_field unique_items false
           "uniqueItems"
           (fun b -> `Bool b)
     | Combine (c, elts) ->
         let combinator = function
           | Any_of -> "anyOf"
           | One_of -> "oneOf"
           | All_of -> "allOf" (* TODO: flatten when possible *)
           | Not -> "not"
         in
         `O [ combinator c,  `A (List.map format_element elts) ]
     | Ref path ->
         `O [ "$ref", `String path ]
     | Def path ->
         `O [ "$ref", `String (String.concat "/" ("#" :: "definitions" :: path)) ]
     | Integer ->
         `O [ "type", `String "integer" ]
     | Number ->
         `O [ "type", `String "number" ]
     | String { pattern ; min_length ; max_length } ->
         `O [ "type", `String "string" ] |>
         neq_field min_length 0
           "minLength"
           (fun i -> `Float (float i)) |>
         option_field max_length
           "maxLength"
           (fun i -> `Float (float i)) |>
         option_field pattern
           "pattern"
           (fun s -> `String s)
     | Boolean ->
         `O [ "type", `String "boolean" ]
     | Null ->
         `O [ "type", `String "null" ]
     | Dummy ->
         invalid_arg "Json_schema.to_json"
     | Any ->
         `O []) |>
    option_field default "default" (fun j -> j) |>
    option_field enum "enum" (fun js -> `A js) |>
    option_field format "format" (fun s -> `String s)|>
    option_field description "description" (fun s -> `String s) |>
    option_field title
      "title"
      (fun s -> `String s)
  in
  match format_element schema.root, schema.definitions with
  | res, [] -> res
  | `O fs, l ->
      let rec build_defs = function
        | Definition (n, elt) :: tl -> (n, format_element elt) :: build_defs tl
        | Group (n, defs) :: tl -> (n, `O (build_defs defs)) :: build_defs tl
        | [] -> []
      in
      `O (("definitions", `O (build_defs l)) :: fs)
  | _ -> assert false

(* JSON encoding -> OCaml definition *)
let of_json json =
  (* parser combinators *)
  let opt_field obj n cb =
    match obj with
    | `O ls ->
        let v = try Some (List.assoc n ls) with Not_found -> None in
        cb v
    | _ -> cb None
  in
  let opt_string_field obj n cb = opt_field obj n @@
    function Some (`String s) -> cb (Some s)
           | Some _ -> None (* type error *)
           | None -> cb None in
  let opt_bool_field def obj n cb = opt_field obj n @@
    function Some (`Bool b) -> cb b
           | Some _ -> None (* type error *)
           | None -> cb def in
  let int f = if floor f = f then Some (int_of_float f) else None in
  let opt_int_field obj n cb = opt_field obj n @@
    function Some (`Float n) -> cb (int n)
           | Some _ -> None (* type error *)
           | None -> cb None in
  let opt_array_field obj n cb = opt_field obj n @@
    function Some (`A s) -> cb (Some s)
           | Some _ -> None (* type error *)
           | None -> cb None in
  (* local resolution of definitions *)
  let collected_definitions = ref [] in
  let rec collect_definition
    : type a. string -> (element_kind -> a option) -> a option = fun uri cb ->
    if String.length uri >= 2
    && String.get uri 0 = '#'
    && String.get uri 1 = '/' then
      (* local ref *)
      let rec find path idx root =
        let stop = try Some (String.index_from uri idx '/') with Not_found -> None in
        match stop with
        | Some stop ->
            let field = String.sub uri idx (stop - idx) in
            find_field field root @@ fun root ->
            find (field :: path) (stop + 1) root
        | None ->
            let field = String.sub uri idx (String.length uri - idx) in
            find_field field root @@ fun root ->
            let name =
              match List.rev (field :: path) with
              | [] -> [ "__root__" ]
              | "definitions" :: tl -> tl
              | tl -> "__root__" :: tl in
            if definition_exists name !collected_definitions then
              cb (Def name)
            else begin
              (* dummy insertion so we don't recurse *)
              collected_definitions := insert_definition name (element Dummy) !collected_definitions ;
              parse_element root @@ fun elt ->
              (* actual insertion *)
              collected_definitions := insert_definition name elt !collected_definitions ;
              cb (Def name)
            end
      and find_field field root cb =
        match root with
        | `O l ->
            begin try cb (List.assoc field l) with
              | Not_found -> None (* local ref not found *)
            end
        | `A l ->
            begin try cb (List.nth l (int_of_string field)) with
              | _ -> None (* local ref not found *)
            end
        | _ -> None (* local ref not found *)
      in find [] 2 (json :> json)
    else
      (* external URI *)
      cb (Ref uri)

  (* recursive parser *)
  and parse_element : type a. json -> (element -> a option) -> a option = fun json cb ->
    let parse_type : type b. (element option -> b option) -> b option = fun cb ->
      opt_field json "type" @@ function
      | Some (`String name) ->
          parse_kind json name @@ fun kind ->
          cb (Some (element kind))
      | Some (`A l) ->
          let rec items acc = function
            | [] ->
                let kind = Combine (Any_of, List.rev acc) in
                cb (Some (element kind))
            | `String name :: tl ->
                parse_kind json name @@ fun kind ->
                let case = element kind in
                items (case :: acc) tl
            | _ :: tl -> None (* type name must be a string *)
          in items [] l
      | Some _ -> None (* type name must be a string *)
      | None -> cb None
    in
    (* reference *)
    let parse_ref : type a. (element option -> a option) -> a option = fun cb ->
      opt_field json "$ref" @@ function
      | Some (`String uri) ->
          collect_definition uri @@ fun path ->
          cb (Some (element path))
      | Some _ ->
          None (* type error *)
      | None -> cb None in
    (* logical combinators *)
    let rec parse_nary name combinator others cb =
      let build = function
        | [] -> cb None (* not found and no auxiliary case *)
        | [ case ] -> cb (Some case) (* one case -> simplify *)
        | cases -> (* several cases build the combination node with empty options *)
            let kind = Combine (combinator, cases) in
            cb (Some (element kind))
      in
      opt_field json name @@ function
      | Some (`A (_ :: _ as cases)) (* list of schemas *) ->
          let rec items acc = function
            | elt :: tl ->
                parse_element elt @@ fun elt ->
                items (elt :: acc) tl
            | [] ->
                build (others @ List.rev acc)
          in items [] cases
      | None ->
          build others
      | _ -> None  in
    (* (unary) schema negation *)
    let rec parse_not cb =
      opt_field json "not" @@ function
      | None -> cb None
      | Some elt ->
          parse_element elt @@ fun elt ->
          let kind = Combine (Not, [ elt ]) in
          cb (Some (element kind)) in
    (* parse optional fields *)
    opt_string_field json "title" @@ fun title ->
    opt_string_field json "description" @@ fun description ->
    opt_field json "default" @@ fun default ->
    opt_array_field json "enum" @@ fun enum ->
    opt_string_field json "format" @@ fun format ->
    (* combine all specifications under a big conjunction *)
    parse_type @@ fun r_type ->
    parse_ref @@ fun r_ref ->
    parse_not @@ fun r_not ->
    parse_nary "oneOf" One_of [] @@ fun r_one_of ->
    parse_nary "anyOf" Any_of [] @@ fun r_any_of ->
    let (@::) e l = match e with None -> l | Some e -> e :: l in
    let cases = r_type @:: r_ref @:: r_not @:: r_one_of @:: r_any_of @:: [] in
    parse_nary "allOf" All_of cases @@ function
    | None ->
        (* no type, ref or logical combination found *)
        cb { title ; description ; default ; format ; kind = Any ; enum }
    | Some { kind } ->
        (* rewrite the result with optional fields *)
        cb { title ; description ; default ; format ; kind ; enum }

  and parse_kind
    : type a. json -> string -> (element_kind -> a option) -> a option = fun json name cb ->
    match name with
    | "integer" -> cb Integer
    | "number" -> cb Number
    | "boolean" -> cb Boolean
    | "null" -> cb Null
    | "string" ->
        let parse_string_specs
          : type a. json -> (string_specs -> a option) -> a option = fun json cb ->
          opt_string_field json "pattern" @@ fun pattern ->
          opt_int_field json "minLength" @@ fun min_length ->
          opt_int_field json "maxLength" @@ fun max_length ->
          let min_length = match min_length with None -> 0 | Some l -> l in
          cb { pattern ; min_length ; max_length } in
        parse_string_specs json @@ fun specs ->
        cb (String specs)
    | "array" ->
        let parse_array_specs
          : type a. json -> (array_specs -> a option) -> a option = fun json cb ->
          opt_bool_field false json "uniqueItems" @@ fun unique_items ->
          opt_int_field json "minItems" @@ fun min_items ->
          opt_int_field json "maxItems" @@ fun max_items ->
          let min_items = match min_items with None -> 0 | Some l -> l in
          opt_field json "additionalItems" @@ function
          | None | Some (`Bool true) ->
              cb { min_items ; max_items ; unique_items ; additional_items = Some (element Any) }
          | Some (`Bool false) ->
              cb { min_items ; max_items ; unique_items ; additional_items = None }
          | Some elt ->
              parse_element elt @@ fun elt ->
              cb { min_items ; max_items ; unique_items ; additional_items = Some elt } in
        let parse_array () =
          opt_field json "items" @@ function
          | Some (`A elts) ->
              let rec elements acc = function
                | [] ->
                    parse_array_specs json @@ fun specs ->
                    cb (Array (List.rev acc, specs))
                | elt :: tl ->
                    parse_element elt @@ fun elt ->
                    elements (elt :: acc) tl
              in elements [] elts
          | Some elt ->
              parse_element elt @@ fun elt ->
              parse_array_specs json @@ fun specs ->
              cb (Monomorphic_array (elt, specs))
          | None ->
              parse_array_specs json @@ fun specs ->
              cb (Monomorphic_array (element Any, specs))
        in parse_array ()
    | "object" ->
        let parse_required : type a. json -> (string list -> a option) -> a option = fun json cb ->
          opt_array_field json "required" @@ function
          | None -> cb []
          | Some l ->
              let rec items acc = function
                | `String s :: tl -> items (s :: acc) tl
                | [] -> cb (List.rev acc)
                | _ -> None (* type error *)
              in items [] l in
        let parse_properties required json cb =
          opt_field json "properties" @@ function
          | Some (`O props) ->
              let rec items acc = function
                | [] -> cb (List.rev acc)
                | (n, elt) :: tl ->
                    parse_element elt @@ fun elt ->
                    let req = List.mem n required in
                    items ((n, elt, req) :: acc) tl
              in items [] props
          | None -> cb []
          | _ -> None (* missing field or type error *) in
        let parse_additional_properties json cb =
          opt_field json "additionalProperties" @@ function
          | Some (`Bool false)->
              cb None
          | None | Some (`Bool true)->
              cb (Some (element Any))
          | Some elt ->
              parse_element elt @@ fun elt ->
              cb (Some elt) in
        let parse_element_assoc field json cb =
          opt_field json field @@ function
          | None -> cb []
          | Some (`O props) ->
              let rec items acc = function
                | [] -> cb (List.rev acc)
                | (n, elt) :: tl ->
                    parse_element elt @@ fun elt ->
                    items ((n, elt) :: acc) tl
              in items [] props
          | Some _ -> None in
        let parse_property_dependencies json cb =
          opt_field json "propertyDependencies" @@ function
          | None -> cb []
          | Some (`O l) ->
              let rec sets sacc = function
                | (n, `A l) :: tl ->
                    let rec strings acc = function
                      | [] -> sets ((n, List.rev acc) :: sacc) tl
                      | `String s :: tl -> strings (s :: acc) tl
                      | _ -> None
                    in strings [] l
                | _ :: tl -> None
                | [] -> cb (List.rev sacc)
              in sets [] l
          | Some _ -> None in
        parse_required json @@ fun required ->
        parse_properties required json @@ fun properties ->
        parse_element_assoc "patternProperties" json @@ fun pattern_properties ->
        parse_additional_properties json @@ fun additional_properties ->
        opt_int_field json "minProperties" @@ fun min_properties ->
        let min_properties = match min_properties with None -> 0 | Some l -> l in
        opt_int_field json "maxProperties" @@ fun max_properties ->
        parse_element_assoc "schemaDependencies" json @@ fun schema_dependencies ->
        parse_property_dependencies json @@ fun property_dependencies ->
        cb (Object { properties ; pattern_properties ;
                     additional_properties ;
                     min_properties ; max_properties ;
                     schema_dependencies ; property_dependencies })
    | _ -> None (* unknown type *)
  in
  parse_element (json :> json) @@ fun root ->
  Some { root ; definitions = !collected_definitions }

(* browe the schema and try to look up every [Def] *)
let check_definitions { root ; definitions } =
  try
    let rec check { kind } = match kind with
      | Object { properties ; pattern_properties ;
                 additional_properties ; schema_dependencies } ->
          List.iter (fun (_, e, _) -> check e) properties ;
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
      | Def path ->
          if not (definition_exists path definitions) then
            raise Not_found
      | Ref _ | String _ | Integer | Number | Boolean | Null | Any | Dummy -> ()
    in check root ; true
  with Not_found -> false

(* box a new root without def *)
let create root =
  let result = { root ; definitions = [] } in
  if not (check_definitions result) then
    invalid_arg "Json_schema.create" ;
  result

(* box a root with existing defs *)
let update root { definitions} =
  let result = { root ; definitions } in
  if not (check_definitions result) then
    invalid_arg "Json_schema.update" ;
  result

(* something to start from *)
let any =
  create (element Any)

(* external ref to speak about schemas inside other schemas *)
let self =
  let version = "http://json-schema.org/draft-04/schema#" in
  { root = element (Ref version) ; definitions = [] }

(* remove unused definitions from the schema *)
let simplify schema =
  let res = ref [] (* collected definitions *) in
  let rec collect { kind } = match kind with
    | Object { properties ; pattern_properties ;
               additional_properties ; schema_dependencies } ->
        List.iter (fun (_, e, _) -> collect e) properties ;
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
    | Def path ->
        let def = find_definition path schema.definitions in
        res := insert_definition path def !res
    | Ref _ | String _ | Integer | Number | Boolean | Null | Any | Dummy -> ()
  in
  collect schema.root ;
  { root = schema.root ; definitions = !res }

(* external definition finding *)
let find_definition path schema =
  find_definition path schema.definitions

(* external definition existence test *)
let definition_exists path schema =
  definition_exists path schema.definitions

(* external definition insertion *)
let add_definition path elt schema =
  { schema with definitions = insert_definition path elt schema.definitions },
  element (Def path)

(* unifies the definitions of two schemas and rewrite them using the
   newly computed set *)
let merge_definitions (sa, sb) =
  let rec merge da db =
    (* we collate the two lists and then sort the result by field name
       so we can just detect duplicates as pairs of consecutive
       elements with the same field name ; we assume that the original
       names do not contain duplicates *)
    let compare da db = match da, db with
      | Group (na, _), Group (nb, _)
      | Group (na, _), Definition (nb, _)
      | Definition (na, _), Group (nb, _)
      | Definition (na, _), Definition (nb, _) ->
          String.compare na nb
    in
    let rec sorted_merge = function
      | (Group (na, la) as a) :: (Group (nb, lb) as b) :: tl ->
          if na = nb then
            Group (na, merge la lb) :: sorted_merge tl
          else
            a :: sorted_merge (b :: tl)
      | (Definition (na, da) as a) :: (Definition (nb, db) as b) :: tl ->
          if na = nb then
            if da = db || da.kind = Dummy || db.kind = Dummy then
              Definition (na, da) :: sorted_merge tl
            else
              invalid_arg "Json_schema.merge_definitions"
          else
            a :: sorted_merge (b :: tl)
      | (Group (na, _) as a) :: (Definition (nb, _) as b) :: tl
      | (Definition (na, _) as a) :: (Group (nb, _) as b) :: tl ->
          if na = nb then
            invalid_arg "Json_schema.merge_definitions"
          else
            a :: sorted_merge (b :: tl)
      | _ :: [] | [] as last -> last
    in
    sorted_merge (List.sort compare (da @ db))
  in
  let definitions = merge sa.definitions sb.definitions in
  { sa with definitions }, { sb with definitions }

(* Combines several schemas *)
let combine op schemas =
  let rec combine sacc eacc = function
    | [] -> update (element (Combine (op, eacc))) sacc
    | s :: ss ->
        let sacc, s = merge_definitions (sacc, s) in
        combine sacc (s.root :: eacc) ss
  in combine any [] schemas
