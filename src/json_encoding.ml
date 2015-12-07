(* JSON structure description using dependently typed combinators. *)

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

(*-- types and errors --------------------------------------------------------*)

exception Unexpected of string * string
exception No_case_matched of exn list
exception Bad_array_size of int * int
exception Missing_field of string
exception Unexpected_field of string
exception Bad_schema of exn
exception Cannot_destruct of (path * exn)

let unexpected kind expected =
  let kind =match kind with
    | `O [] -> "empty object"
    | `A [] -> "empty array"
    | `O _ -> "object"
    | `A _ -> "array"
    | `Null -> "null"
    | `String _ -> "string"
    | `Float _ -> "number"
    | `Bool _ -> "boolean" in
  Cannot_destruct ([], Unexpected (kind, expected))

(* intermediate internal type without the polymorphic variant constraint *)
type _ encoding =
  | Null : unit encoding
  | Int : int encoding
  | Bool : bool encoding
  | String : string encoding
  | Float : float encoding
  | Array : 'a encoding -> 'a array encoding
  | Obj : 'a field -> 'a encoding
  | Objs : 'a encoding * 'b encoding -> ('a * 'b) encoding
  | Tup : 'a encoding -> 'a encoding
  | Tups : 'a encoding * 'b encoding -> ('a * 'b) encoding
  | Custom : ('t -> value) * (value -> 't) * Json_schema.schema -> 't encoding
  | Conv : ('a -> 'b) * ('b -> 'a) * 'b encoding -> 'a encoding
  | Describe : string option * string option * 'a encoding -> 'a encoding
  | Mu : string * ('a encoding -> 'a encoding) -> 'a encoding

and _ field =
  | Req : string * 'a encoding -> 'a field
  | Opt : string * 'a encoding -> 'a option field
  | Dft : string * 'a encoding * 'a -> 'a field

let rec print_error ?print_unknown ppf = function
  | Cannot_destruct ([], exn) ->
    print_error ?print_unknown ppf exn
  | Cannot_destruct (path, Unexpected (unex, ex)) ->
    Format.fprintf ppf
      "At %a, unexpected %s instead of %s"
      (print_path_as_json_path ~wildcards:true) path
      unex ex
  | Cannot_destruct (path, No_case_matched errs) ->
    Format.fprintf ppf
      "@[<v 2>At %a, no case matched:@,%a@]"
      (print_path_as_json_path ~wildcards:true) path
      (Format.pp_print_list (print_error ?print_unknown)) errs
  | Cannot_destruct (path, Bad_array_size (unex, ex)) ->
    Format.fprintf ppf
      "At %a, unexpected array of size %d instead of %d"
      (print_path_as_json_path ~wildcards:true) path
      unex ex
  | Cannot_destruct (path, Missing_field n) ->
    Format.fprintf ppf
      "At %a, missing object field %s"
      (print_path_as_json_path ~wildcards:true) path
      n
  | Cannot_destruct (path, Unexpected_field n) ->
    Format.fprintf ppf
      "At %a, unexpected object field %s"
      (print_path_as_json_path ~wildcards:true) path
      n
  | Cannot_destruct (path, Bad_schema exn) ->
    Format.fprintf ppf
      "@[<v 2>At %a, bad custom schema:@,%a@]"
      (print_path_as_json_path ~wildcards:true) path
      (print_error ?print_unknown) exn
  | Unexpected (unex, ex) ->
    Format.fprintf ppf
      "Unexpected %s instead of %s" unex ex
  | No_case_matched errs ->
    Format.fprintf ppf
      "@[<v 2>No case matched:@,%a@]"
      (Format.pp_print_list (print_error ?print_unknown)) errs
  | Bad_array_size (unex, ex) ->
    Format.fprintf ppf
      "Unexpected array of size %d instead of %d" unex ex
  | Missing_field n ->
    Format.fprintf ppf
      "Missing object field %s" n
  | Unexpected_field n ->
    Format.fprintf ppf
      "Unexpected object field %s" n
  | Bad_schema exn ->
    Format.fprintf ppf
      "@[<v 2>bad custom schema:@,%a@]"
      (print_error ?print_unknown) exn
  | Cannot_destruct (path, exn) ->
    Format.fprintf ppf
      "@[<v 2>At %a:@,%a@]"
      (print_path_as_json_path ~wildcards:true) path
      (print_error ?print_unknown) exn
  | exn ->
    Json_schema.print_error ?print_unknown ppf exn

(*-- construct / destruct / schema over the nain GADT forms ------------------*)

let construct enc v =
  let rec construct
    : type t. t encoding -> t -> value
    = function
      | Null -> (fun () -> `Null)
      | Int -> (fun (i : t) -> `Float (float  i))
      | Bool -> (fun (b : t) -> `Bool b)
      | String -> (fun s -> `String s)
      | Float -> (fun f -> `Float f)
      | Describe (_, _, t) -> construct t
      | Custom (w, _, _) -> (fun (j : t) -> w j)
      | Conv (ffrom, _, t) -> (fun v -> construct t (ffrom v))
      | Mu (name, self) -> construct (self (Mu (name, self)))
      | Array t ->
        let w v = (construct t v :> value) in
        (fun arr -> `A (Array.to_list (Array.map w arr)))
      | Obj (Req (n, t)) ->
        let w v = (construct t v :> value) in
        (fun v -> `O [ n, w v ])
      | Obj (Dft (n, t, d)) ->
        let w v = (construct t v :> value) in
        (fun v -> `O (if v = d then [ n, w v ] else []))
      | Obj (Opt (n, t)) ->
        let w v = (construct t v :> value) in
        (function None -> `O [] | Some v -> `O [ n, w v ])
      | Objs (o1, o2) ->
        let w1 v = construct o1 v in
        let w2 v = construct o2 v in
        (function (v1, v2) ->
        match w1 v1, w2 v2 with
        | `O l1, `O l2 -> `O (l1 @ l2)
        | _ -> invalid_arg "Json_encoding.construct: consequence of bad merge_objs")
      | Tup t ->
        let w v = (construct t v :> value) in
        (fun v -> `A [ w v ])
      | Tups (o1, o2) ->
        let w1 v = construct o1 v in
        let w2 v = construct o2 v in
        (function (v1, v2) ->
        match w1 v1, w2 v2 with
        | `A l1, `A l2 -> `A (l1 @ l2)
        | _ -> invalid_arg "Json_encoding.construct: consequence of bad merge_tups") in
  (construct enc v : value :> [> value ])

let rec destruct
  : type t. t encoding -> (value -> t)
  = function
    | Null -> (function `Null -> () | k -> raise (unexpected k "null"))
    | Int -> (function `Float f -> int_of_float f | k -> raise (unexpected k "number"))
    | Bool -> (function `Bool b -> (b : t) | k -> raise (unexpected k "boolean"))
    | String -> (function `String s -> s | k -> raise (unexpected k "string"))
    | Float -> (function `Float f -> f | k -> raise (unexpected k "float"))
    | Describe (_, _, t) -> destruct t
    | Custom (_, r, _) -> r
    | Conv (_, fto, t) -> (fun v -> fto (destruct t v))
    | Mu (name, self) -> destruct (self (Mu (name, self)))
    | Array t ->
      (function
        | `A cells ->
          Array.mapi
            (fun i cell ->
               try destruct t cell with Cannot_destruct (path, err) ->
                 raise (Cannot_destruct (`Index i :: path, err)))
            (Array.of_list cells)
        | k -> raise @@ unexpected k "array")
    | Obj (Req (n, t)) ->
      (function
        | `O fields ->
          (try destruct t (List.assoc n fields) with
           | Not_found ->
             raise (Cannot_destruct ([], Missing_field n))
           | Cannot_destruct (path, err) ->
             raise (Cannot_destruct (`Field n :: path, err)))
        | k -> raise @@ unexpected k "object")
    | Obj (Opt (n, t)) ->
      (function
        | `O fields ->
          (try Some (destruct t (List.assoc n fields)) with
           | Not_found -> None
           | Cannot_destruct (path, err) ->
             raise (Cannot_destruct (`Field n :: path, err)))
        | k -> raise @@ unexpected k "object")
    | Obj (Dft (n, t, d)) ->
      (function
        | `O fields ->
          (try destruct t (List.assoc n fields) with
           | Not_found -> d
           | Cannot_destruct (path, err) ->
             raise (Cannot_destruct (`Field n :: path, err)))
        | k -> raise @@ unexpected k "object")
    | Objs (o1, o2) ->
      (fun j -> destruct o1 j, destruct o2 j)
    | Tup _ as t ->
      let r, i = destruct_tup 0 t in
      (function
        | `A cells ->
          let cells = Array.of_list cells in
          let len = Array.length cells in
          if i <> Array.length cells then
            raise (Cannot_destruct ([], Bad_array_size (len, i)))
          else r cells
        | k -> raise @@ unexpected k "array")
    | Tups _ as t ->
      let r, i = destruct_tup 0 t in
      (function
        | `A cells ->
          let cells = Array.of_list cells in
          let len = Array.length cells in
          if i <> Array.length cells then
            raise (Cannot_destruct ([], Bad_array_size (len, i)))
          else r cells
        | k -> raise @@ unexpected k "array")
and destruct_tup
  : type t. int -> t encoding -> (value array -> t) * int
  = fun i t -> match t with
    | Tup t ->
      (fun arr ->
         (try destruct t arr.(i) with Cannot_destruct (path, err) ->
           raise (Cannot_destruct (`Index i :: path, err)))), succ i
    | Tups (t1, t2) ->
      let r1, i = destruct_tup i t1 in
      let r2, i = destruct_tup i t2 in
      (fun arr -> r1 arr, r2 arr), i
    | _ -> invalid_arg "Json_typed.destruct: consequence of bad merge_tups"

let destruct t =
  let d = destruct t in
  fun j -> d (j :> value)

let schema encoding =
  let open Json_schema in
  let sch = ref any in
  let rec object_schema
    : type t. t encoding -> (string * element * bool * value option) list
    = function
      | Conv (_, _, o) -> object_schema o
      | Obj (Req (n, t)) -> [ n, schema t, true, None ]
      | Obj (Opt (n, t)) -> [ n, schema t, false, None ]
      | Obj (Dft (n, t, d)) -> [ n, schema t, false, Some (construct t d :> value)]
      | Objs (o1, o2) -> object_schema o1 @ object_schema o2
      | _ -> invalid_arg "Json_typed.schema: consequence of bad merge_objs"
  and array_schema
    : type t. t encoding -> element list
    = function
      | Conv (_, _, o) -> array_schema o
      | Tup t -> [ schema t ]
      | Tups (t1, t2) -> array_schema t1 @ array_schema t2
      | _ -> invalid_arg "Json_typed.schema: consequence of bad merge_tups"
  and schema
    : type t. t encoding -> element
    = function
      | Null -> element Null
      | Int -> element Integer
      | Bool -> element Boolean
      | String -> element (String string_specs)
      | Float -> element Number
      | Describe (title, description, t) ->
        { (schema t) with title ; description }
      | Custom (_, _, s) ->
        sch := fst (merge_definitions (!sch, s)) ;
        root s
      | Conv (_, _, t) -> schema t
      | Mu (name, f) ->
        let fake_schema =
          let sch, elt = add_definition name (element Dummy) any in
          update elt sch in
        let fake_self =
          Custom ((fun _ -> assert false),
                  (fun _ -> assert false),
                  fake_schema) in
        let nsch, def = add_definition name (schema (f fake_self)) !sch in
        sch := nsch ; def
      | Array t ->
        element (Monomorphic_array (schema t, array_specs))
      | Obj _ as o -> element (Object { object_specs with properties = object_schema o })
      | Objs _ as o -> element (Object { object_specs with properties = object_schema o })
      | Tup _ as t -> element (Array (array_schema t, array_specs))
      | Tups _ as t -> element (Array (array_schema t, array_specs)) in
  update (schema encoding) !sch

(*-- utility wrappers over the GADT ------------------------------------------*)

let req ?title ?description n t = Req (n, Describe (title, description, t))
let opt ?title ?description n t = Opt (n, Describe (title, description, t))
let dft ?title ?description n t d = Dft (n, Describe (title, description, t), d)

let mu name self = Mu (name, self)
let null = Null
let int = Int
let float = Float
let string = String
let conv ffrom fto ?schema t =
  match schema with
  | None ->
    Conv (ffrom, fto, t)
  | Some schema ->
    let ffrom j = construct t (ffrom j) in
    let fto v = fto (destruct t v) in
    Custom (ffrom, fto, schema)
let bytes = Conv (Bytes.to_string, Bytes.of_string, string)
let bool = Bool
let array t = Array t
let obj1 f1 = Obj f1
let obj2 f1 f2 = Objs (Obj f1, Obj f2)
let obj3 f1 f2 f3 =
  Conv
    ((fun (a, b, c) -> (a, (b, c))),
     (fun (a, (b, c)) -> (a, b, c)),
     Objs (Obj f1, Objs (Obj f2, Obj f3)))
let obj4 f1 f2 f3 f4 =
  Conv
    ((fun (a, b, c, d) -> (a, (b, (c, d)))),
     (fun (a, (b, (c, d))) -> (a, b, c, d)),
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Obj f4))))
let obj5 f1 f2 f3 f4 f5 =
  Conv
    ((fun (a, b, c, d, e) -> (a, (b, (c, (d, e))))),
     (fun (a, (b, (c, (d, e)))) -> (a, b, c, d, e)),
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Obj f5)))))
let obj6 f1 f2 f3 f4 f5 f6 =
  Conv
    ((fun (a, b, c, d, e, f) -> (a, (b, (c, (d, (e, f)))))),
     (fun (a, (b, (c, (d, (e, f))))) -> (a, b, c, d, e, f)),
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, Obj f6))))))
let obj7 f1 f2 f3 f4 f5 f6 f7 =
  Conv
    ((fun (a, b, c, d, e, f, g) -> (a, (b, (c, (d, (e, (f, g))))))),
     (fun (a, (b, (c, (d, (e, (f, g)))))) -> (a, b, c, d, e, f, g)),
     let rest = Objs (Obj f6, Obj f7) in
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))
let obj8 f1 f2 f3 f4 f5 f6 f7 f8 =
  Conv
    ((fun (a, b, c, d, e, f, g, h) -> (a, (b, (c, (d, (e, (f, (g, h)))))))),
     (fun (a, (b, (c, (d, (e, (f, (g, h))))))) -> (a, b, c, d, e, f, g, h)),
     let rest = Objs (Obj f6, Objs (Obj f7, Obj f8)) in
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))
let obj9 f1 f2 f3 f4 f5 f6 f7 f8 f9 =
  Conv
    ((fun (a, b, c, d, e, f, g, h, i) -> (a, (b, (c, (d, (e, (f, (g, (h, i))))))))),
     (fun (a, (b, (c, (d, (e, (f, (g, (h, i)))))))) -> (a, b, c, d, e, f, g, h, i)),
     let rest = Objs (Obj f6, Objs (Obj f7, Objs (Obj f8, Obj f9))) in
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))
let obj10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 =
  Conv
    ((fun (a, b, c, d, e, f, g, h, i, j) -> (a, (b, (c, (d, (e, (f, (g, (h, (i, j)))))))))),
     (fun (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))) -> (a, b, c, d, e, f, g, h, i, j)),
     let rest = Objs (Obj f6, Objs (Obj f7, Objs (Obj f8, Objs (Obj f9, Obj f10)))) in
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))
let tup1 f1 = Tup f1
let tup2 f1 f2 = Tups (Tup f1, Tup f2)
let tup3 f1 f2 f3 =
  Conv
    ((fun (a, b, c) -> (a, (b, c))),
     (fun (a, (b, c)) -> (a, b, c)),
     Tups (Tup f1, Tups (Tup f2, Tup f3)))
let tup4 f1 f2 f3 f4 =
  Conv
    ((fun (a, b, c, d) -> (a, (b, (c, d)))),
     (fun (a, (b, (c, d))) -> (a, b, c, d)),
     Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tup f4))))
let tup5 f1 f2 f3 f4 f5 =
  Conv
    ((fun (a, b, c, d, e) -> (a, (b, (c, (d, e))))),
     (fun (a, (b, (c, (d, e)))) -> (a, b, c, d, e)),
     Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tup f5)))))
let tup6 f1 f2 f3 f4 f5 f6 =
  Conv
    ((fun (a, b, c, d, e, f) -> (a, (b, (c, (d, (e, f)))))),
     (fun (a, (b, (c, (d, (e, f))))) -> (a, b, c, d, e, f)),
     Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, Tup f6))))))
let tup7 f1 f2 f3 f4 f5 f6 f7 =
  Conv
    ((fun (a, b, c, d, e, f, g) -> (a, (b, (c, (d, (e, (f, g))))))),
     (fun (a, (b, (c, (d, (e, (f, g)))))) -> (a, b, c, d, e, f, g)),
     let rest = Tups (Tup f6, Tup f7) in
     Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, rest))))))
let tup8 f1 f2 f3 f4 f5 f6 f7 f8 =
  Conv
    ((fun (a, b, c, d, e, f, g, h) -> (a, (b, (c, (d, (e, (f, (g, h)))))))),
     (fun (a, (b, (c, (d, (e, (f, (g, h))))))) -> (a, b, c, d, e, f, g, h)),
     let rest = Tups (Tup f6, Tups (Tup f7, Tup f8)) in
     Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, rest))))))
let tup9 f1 f2 f3 f4 f5 f6 f7 f8 f9 =
  Conv
    ((fun (a, b, c, d, e, f, g, h, i) -> (a, (b, (c, (d, (e, (f, (g, (h, i))))))))),
     (fun (a, (b, (c, (d, (e, (f, (g, (h, i)))))))) -> (a, b, c, d, e, f, g, h, i)),
     let rest = Tups (Tup f6, Tups (Tup f7, Tups (Tup f8, Tup f9))) in
     Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, rest))))))
let tup10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 =
  Conv
    ((fun (a, b, c, d, e, f, g, h, i, j) -> (a, (b, (c, (d, (e, (f, (g, (h, (i, j)))))))))),
     (fun (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))) -> (a, b, c, d, e, f, g, h, i, j)),
     let rest = Tups (Tup f6, Tups (Tup f7, Tups (Tup f8, Tups (Tup f9, Tup f10)))) in
     Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, rest))))))

let custom w r ~schema = Custom (w, r, schema)
let describe ?title ?description t = Describe (title, description, t)
let string_enum cases =
  let specs = Json_schema.({ pattern = None ; min_length = 0 ; max_length = None }) in
  let enum = List.map (fun (s, _) -> `String s) cases in
  let rcases = List.map (fun (s, c) -> (c, s)) cases in
  conv
    (fun v -> try List.assoc v rcases with Not_found ->
       invalid_arg "Json_typed.string_enum")
    (fun s ->
       (try List.assoc s cases with Not_found ->
         let rec orpat ppf = function
           | [] -> assert false
           | [ last, _ ] -> Format.fprintf ppf "%S" last
           | [ prev, _ ; last, _ ] -> Format.fprintf ppf "%S or %S" prev last
           | (prev, _) :: rem -> Format.fprintf ppf "%S , %a" prev orpat rem in
         let unexpected = Format.asprintf "string value %S" s in
         let expected = Format.asprintf "%a" orpat cases in
         raise (Cannot_destruct ([], Unexpected (unexpected, expected)))))
    ~schema: Json_schema.(update { (element (String specs)) with enum = Some enum } any)
    string

let def name encoding =
  Custom
    (construct encoding,
     destruct encoding,
     (let open Json_schema in
      let sch = schema encoding in
      let sch, def = add_definition name (root sch) sch in
      update def sch))

let assoc : type t. t encoding -> (string * t) list encoding = fun t ->
  Custom
    ((fun l -> `O (List.map (fun (n, v) -> n, (construct t v :> value)) l)),
     (function
       | `O l ->
         let destruct n t v = try
             destruct t v
           with Cannot_destruct (p, exn) -> raise (Cannot_destruct (`Field n :: p, exn)) in
         List.map (fun (n, v) -> n, destruct n t v) l
       | #value as k -> raise (unexpected k "asssociative object")),
     let s = schema t in
     Json_schema.(update (element (Object { object_specs with additional_properties = Some (root s)})) s))

let option : type t. t encoding -> t option encoding = fun t ->
  Custom
    ((function None -> `Null | Some v -> (construct t v :> value)),
     (function `Null -> None | j -> Some (destruct t j)),
     let s = schema t in
     Json_schema.(update (element (Combine (One_of, [(root s) ; element Null]))) s))

let int32 =
  Conv (Int32.to_float, Int32.of_float, float)

let any_value =
  Custom
    ((fun value -> value),
     (fun value -> value),
     Json_schema.any)

let any_document =
  Custom
    ((fun value -> value),
     (function `A _ | `O _ as d -> d | k -> raise @@ unexpected k "array or object"),
     Json_schema.any)

let any_schema =
  Custom
    (Json_schema.to_json,
     (fun j -> try Json_schema.of_json j with err ->
        raise (Cannot_destruct ([], Bad_schema err))),
     Json_schema.self)

let merge_tups t1 t2 =
  Tups (t1, t2)
let list t =
  Conv (Array.of_list, Array.to_list, Array t)

let merge_objs o1 o2 =
  Objs (o1, o2)
let empty =
  Custom
    ((fun () -> `O []),
     (function
       | `O [] -> ()
       | `O [ f, _] -> raise (Cannot_destruct ([], Unexpected_field f))
       | k -> raise @@ unexpected k "an empty object"),
     Json_schema.(create (element (Object { properties = [] ;
                                            pattern_properties = [] ;
                                            additional_properties = None ;
                                            min_properties = 0 ;
                                            max_properties = Some 0 ;
                                            schema_dependencies = [] ;
                                            property_dependencies = [] }))))
let unit =
  Custom
    ((fun () -> `O []),
     (function
       | _ -> ()),
     Json_schema.any)

type 't case = Case : 'a encoding * ('t -> 'a option) * ('a -> 't) -> 't case

let case encoding fto ffrom =
  Case (encoding, fto, ffrom)

let union = function
  | [] -> invalid_arg "Json_typed.union"
  | l ->
    Custom
      ((fun v ->
          let rec do_cases = function
            | [] -> invalid_arg "Json_typed.union"
            | Case (encoding, fto, _) :: rest ->
              match fto v with
              | Some v -> (construct encoding v :> value)
              | None -> do_cases rest in
          do_cases l),
       (fun v ->
          let rec do_cases errs = function
            | [] -> raise (Cannot_destruct ([], No_case_matched (List.rev errs)))
            | Case (encoding, _, ffrom) :: rest ->
              try ffrom (destruct encoding v) with
                err -> do_cases (err :: errs) rest in
          do_cases [] l),
       Json_schema.combine
         Json_schema.One_of
         (List.map (fun (Case (encoding, _, _)) -> schema encoding) l))
