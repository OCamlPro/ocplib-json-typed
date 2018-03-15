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

exception Unexpected of string * string
exception No_case_matched of exn list
exception Bad_array_size of int * int
exception Missing_field of string
exception Unexpected_field of string
exception Bad_schema of exn
exception Cannot_destruct of (Json_query.path * exn)

(*-- types and errors --------------------------------------------------------*)

let unexpected kind expected =
  let kind = match kind with
    | `O [] -> "empty object"
    | `A [] -> "empty array"
    | `O _ -> "object"
    | `A _ -> "array"
    | `Null -> "null"
    | `String _ -> "string"
    | `Float _ -> "number"
    | `Bool _ -> "boolean" in
  Cannot_destruct ([], Unexpected (kind, expected))

type 't repr_agnostic_custom =
  { write : 'rt. (module Json_repr.Repr with type value = 'rt) -> 't -> 'rt ;
    read : 'rf. (module Json_repr.Repr with type value = 'rf) -> 'rf -> 't }

(* The GADT definition for encodings. This type must be kept internal
    because it does not encode all invariants. Some properties are
    checked at encoding construction time by smart constructors, since
    checking them would either be impossible, or would make the type
    too complex. In a few corners that involve custom encodings using
    user defined functions, some properties cannot be checked until
    construction/destruction time. If such a run time check fails, is
    denotes a programmer error and an [Invalid_argument] exceptions is
    thus raised. *)
type _ encoding =
  | Null : unit encoding
  | Empty : unit encoding
  | Ignore : unit encoding
  | Constant : string -> unit encoding
  | Int : 'a int_encoding -> 'a encoding
  | Bool : bool encoding
  | String : string encoding
  | Float : bounds option -> float encoding
  | Array : 'a encoding -> 'a array encoding
  | Obj : 'a field -> 'a encoding
  | Objs : 'a encoding * 'b encoding -> ('a * 'b) encoding
  | Tup : 'a encoding -> 'a encoding
  | Tups : 'a encoding * 'b encoding -> ('a * 'b) encoding
  | Custom : 't repr_agnostic_custom * Json_schema.schema -> 't encoding
  | Conv : ('a -> 'b) * ('b -> 'a) * 'b encoding * Json_schema.schema option -> 'a encoding
  | Describe : string option * string option * 'a encoding -> 'a encoding
  | Mu : string * ('a encoding -> 'a encoding) -> 'a encoding
  | Union : 't case list -> 't encoding

and 'a int_encoding =
  { name : string ;
    of_float : float -> 'a ;
    to_float : 'a -> float ;
    lower_bound : 'a ;
    upper_bound : 'a }

and bounds = { minimum : float ;
               maximum : float }

and _ field =
  | Req : string * 'a encoding -> 'a field
  | Opt : string * 'a encoding -> 'a option field
  | Dft : string * 'a encoding * 'a -> 'a field

and 't case =
  | Case : 'a encoding * ('t -> 'a option) * ('a -> 't) -> 't case

(*-- construct / destruct / schema over the main GADT forms ------------------*)

module Make (Repr : Json_repr.Repr) = struct

  let construct enc v =
    let rec construct
      : type t. t encoding -> t -> Repr.value
      = function
        | Null -> (fun () -> Repr.repr `Null)
        | Empty -> (fun () -> Repr.repr (`O []))
        | Ignore -> (fun () -> Repr.repr (`O []))
        | Constant str -> (fun () -> Repr.repr (`String str))
        | Int { name ; to_float ; lower_bound ; upper_bound } ->
          let err = "Json_encoding.construct: " ^ name ^ " out of range" in
          (fun (i : t) ->
             if i < lower_bound || i > upper_bound then invalid_arg err ;
             Repr.repr (`Float (to_float  i)))
        | Bool -> (fun (b : t) -> Repr.repr (`Bool b))
        | String -> (fun s -> Repr.repr (`String s))
        | Float (Some { minimum ; maximum }) ->
          let err = "Json_encoding.construct: float out of range" in
          (fun float ->
             if float < minimum || float > maximum then invalid_arg err ;
             Repr.repr (`Float float))
        | Float None -> (fun float -> Repr.repr (`Float float))
        | Describe (_, _, t) -> construct t
        | Custom ({ write }, _) -> (fun (j : t) -> write (module Repr) j)
        | Conv (ffrom, _, t, _) -> (fun v -> construct t (ffrom v))
        | Mu (name, self) -> construct (self (Mu (name, self)))
        | Array t ->
          let w v = construct t v in
          (fun arr -> Repr.repr (`A (Array.to_list (Array.map w arr))))
        | Obj (Req (n, t)) ->
          let w v = construct t v in
          (fun v -> Repr.repr (`O [ n, w v ]))
        | Obj (Dft (n, t, d)) ->
          let w v = construct t v in
          (fun v -> Repr.repr (`O (if v <> d then [ n, w v ] else [])))
        | Obj (Opt (n, t)) ->
          let w v = construct t v in
          (function None -> Repr.repr (`O []) | Some v -> Repr.repr (`O [ n, w v ]))
        | Objs (o1, o2) ->
          let w1 v = construct o1 v in
          let w2 v = construct o2 v in
          (function (v1, v2) ->
           match Repr.view (w1 v1), Repr.view (w2 v2) with
           | `O l1, `O l2 -> Repr.repr (`O (l1 @ l2))
           | `Null, `Null
           | _ -> invalid_arg "Json_encoding.construct: consequence of bad merge_objs")
        | Tup t ->
          let w v = construct t v in
          (fun v -> Repr.repr (`A [ w v ]))
        | Tups (o1, o2) ->
          let w1 v = construct o1 v in
          let w2 v = construct o2 v in
          (function (v1, v2) ->
           match Repr.view (w1 v1), Repr.view (w2 v2) with
           | `A l1, `A l2 -> Repr.repr (`A (l1 @ l2))
           | _ -> invalid_arg "Json_encoding.construct: consequence of bad merge_tups")
        | Union cases ->
          (fun v ->
             let rec do_cases = function
               | [] -> invalid_arg "Json_encoding.construct: consequence of bad union"
               | Case (encoding, fto, _) :: rest ->
                 match fto v with
                 | Some v -> construct encoding v
                 | None -> do_cases rest in
             do_cases cases) in
    construct enc v

  let rec destruct
    : type t. t encoding -> (Repr.value -> t)
    = function
      | Null -> (fun v -> match Repr.view v with `Null -> () | k -> raise (unexpected k "null"))
      | Empty -> (fun v -> match Repr.view v with
          | `O [] -> ()
          | `O [ f, _] -> raise (Cannot_destruct ([], Unexpected_field f))
          | k -> raise @@ unexpected k "an empty object")
      | Ignore -> (fun v -> match Repr.view v with _ -> ())
      | Constant str ->
        (fun v ->
           match Repr.view v with
           | `String s when s = str -> ()
           | x -> raise @@ unexpected x str)
      | Int { name ; of_float ; to_float ; lower_bound ; upper_bound } ->
        let lower_bound = to_float lower_bound in
        let upper_bound = to_float upper_bound in
        (fun v ->
           match Repr.view v with
           | `Float v ->
             let rest, v = modf v in
             if rest <> 0. then begin
               let exn = Failure (name ^ " cannot have a fractional part") in
               raise (Cannot_destruct ([], exn))
             end ;
             if v < lower_bound || v > upper_bound then begin
               let exn = Failure (name ^ " out of range") in
               raise (Cannot_destruct ([], exn))
             end ;
             of_float v
           | k -> raise (unexpected k "number"))
      | Bool -> (fun v -> match Repr.view v with `Bool b -> (b : t) | k -> raise (unexpected k "boolean"))
      | String -> (fun v -> match Repr.view v with `String s -> s | k -> raise (unexpected k "string"))
      | Float None -> (fun v -> match Repr.view v with `Float f -> f | k -> raise (unexpected k "float"))
      | Float (Some { minimum ; maximum }) ->
        (fun v ->
           match Repr.view v with
           | `Float f ->
             if f < minimum || f > maximum
             then
               let exn = Failure ("float out of range") in
               raise (Cannot_destruct ([], exn))
             else f
           | k -> raise (unexpected k "float"))
      | Describe (_, _, t) -> destruct t
      | Custom ({ read }, _) -> read (module Repr)
      | Conv (_, fto, t, _) -> (fun v -> fto (destruct t v))
      | Mu (name, self) -> destruct (self (Mu (name, self)))
      | Array t ->
        (fun v -> match Repr.view v with
           | `O [] ->
             (* Weak `Repr`s like BSON don't know the difference  *)
             [||]
           | `A cells ->
             Array.mapi
               (fun i cell ->
                  try destruct t cell with Cannot_destruct (path, err) ->
                    raise (Cannot_destruct (`Index i :: path, err)))
               (Array.of_list cells)
           | k -> raise @@ unexpected k "array")
      | Obj _  as t ->
        let d = destruct_obj t in
        (fun v -> match Repr.view v with
           | `O fields ->
             let r, rest, ign = d fields in
             begin match rest with
               | (field, _) :: _ when not ign -> raise @@ Unexpected_field field
               | _ -> r
             end
           | k -> raise @@ unexpected k "object")
      | Objs _ as t ->
        let d = destruct_obj t in
        (fun v -> match Repr.view v with
           | `O fields ->
             let r, rest, ign = d fields in
             begin match rest with
               | (field, _) :: _ when not ign -> raise @@ Unexpected_field field
               | _ -> r
             end
           | k -> raise @@ unexpected k "object")
      | Tup _ as t ->
        let r, i = destruct_tup 0 t in
        (fun v -> match Repr.view v with
           | `A cells ->
             let cells = Array.of_list cells in
             let len = Array.length cells in
             if i <> Array.length cells then
               raise (Cannot_destruct ([], Bad_array_size (len, i)))
             else r cells
           | k -> raise @@ unexpected k "array")
      | Tups _ as t ->
        let r, i = destruct_tup 0 t in
        (fun v -> match Repr.view v with
           | `A cells ->
             let cells = Array.of_list cells in
             let len = Array.length cells in
             if i <> Array.length cells then
               raise (Cannot_destruct ([], Bad_array_size (len, i)))
             else r cells
           | k -> raise @@ unexpected k "array")
      | Union cases ->
        (fun v ->
           let rec do_cases errs = function
             | [] -> raise (Cannot_destruct ([], No_case_matched (List.rev errs)))
             | Case (encoding, _, ffrom) :: rest ->
               try ffrom (destruct encoding v) with
                 err -> do_cases (err :: errs) rest in
           do_cases [] cases)
  and destruct_tup
    : type t. int -> t encoding -> (Repr.value array -> t) * int
    = fun i t -> match t with
      | Tup t ->
        (fun arr ->
           (try destruct t arr.(i) with Cannot_destruct (path, err) ->
              raise (Cannot_destruct (`Index i :: path, err)))), succ i
      | Tups (t1, t2) ->
        let r1, i = destruct_tup i t1 in
        let r2, i = destruct_tup i t2 in
        (fun arr -> r1 arr, r2 arr), i
      | Conv (ffrom, fto, t, _) ->
        let r, i = destruct_tup i t in
        (fun arr -> fto (r arr)), i
      | Mu (_, self) as mu -> destruct_tup i (self mu)
      | _ -> invalid_arg "Json_encoding.destruct: consequence of bad merge_tups"
  and destruct_obj
    : type t. t encoding -> (string * Repr.value) list -> t * (string * Repr.value) list * bool
    = fun t ->
      let rec assoc acc n = function
        | [] -> raise Not_found
        | (f, v) :: rest when n = f -> v, acc @ rest
        | oth :: rest -> assoc (oth :: acc) n rest in
      match t with
      | Empty -> (fun fields -> (), fields, false)
      | Ignore -> (fun fields -> (), fields, true)
      | Obj (Req (n, t)) ->
        (fun fields ->
           try
             let v, rest = assoc [] n fields in
             destruct t v, rest, false
           with
           | Not_found ->
             raise (Cannot_destruct ([], Missing_field n))
           | Cannot_destruct (path, err) ->
             raise (Cannot_destruct (`Field n :: path, err)))
      | Obj (Opt (n, t)) ->
        (fun fields ->
           try
             let v, rest = assoc [] n fields in
             Some (destruct t v), rest, false
           with
           | Not_found -> None, fields, false
           | Cannot_destruct (path, err) ->
             raise (Cannot_destruct (`Field n :: path, err)))
      | Obj (Dft (n, t, d)) ->
        (fun fields ->
           try
             let v, rest = assoc [] n fields in
             destruct t v, rest, false
           with
           | Not_found -> d, fields, false
           | Cannot_destruct (path, err) ->
             raise (Cannot_destruct (`Field n :: path, err)))
      | Objs (o1, o2) ->
        let d1 = destruct_obj o1 in
        let d2 = destruct_obj o2 in
        (fun fields ->
           let r1, rest, ign1 = d1 fields in
           let r2, rest, ign2 = d2 rest in
           (r1, r2), rest, ign1 || ign2)
      | Conv (_, fto, t, _) ->
        let d = destruct_obj t in
        (fun fields ->
           let r, rest, ign = d fields in
           fto r, rest, ign)
      | Mu (_, self) as mu -> destruct_obj (self mu)
      | Union cases ->
        (fun fields ->
           let rec do_cases errs = function
             | [] -> raise (Cannot_destruct ([], No_case_matched (List.rev errs)))
             | Case (encoding, _, ffrom) :: rest ->
               try
                 let r, rest, ign = destruct_obj encoding fields in
                 ffrom r, rest, ign
               with err -> do_cases (err :: errs) rest in
           do_cases [] cases)
      | _ -> invalid_arg "Json_encoding.destruct: consequence of bad merge_objs"

  let custom write read ~schema =
    let read
      : type tf. (module Json_repr.Repr with type value = tf) -> tf -> 't
      = fun (module Repr_f) repr ->
        read (Json_repr.convert (module Repr_f) (module Repr) repr) in
    let write
      : type tf. (module Json_repr.Repr with type value = tf) -> 't -> tf
      = fun (module Repr_f) v ->
        Json_repr.convert (module Repr) (module Repr_f) (write v) in
    Custom ({ read ; write }, schema)
end

module Ezjsonm_encoding = Make (Json_repr.Ezjsonm)

let schema encoding =
  let open Json_schema in
  let sch = ref any in
  let (@@@) (l1, b1) (l2, b2) =
    l1 @ l2, b1 || b2  in
  let rec (@@) l1 l2 = match l1 with
    | [] -> []
    | e :: es -> (List.map (fun l -> e @@@ l) l2) @ (es @@ l2) in
  let rec object_schema
    : type t. t encoding -> ((string * element * bool * Json_repr.any option) list * bool) list
    = function
      | Conv (_, _, o, None) -> object_schema o
      | Empty -> [ [], false ]
      | Ignore -> [ [], true ]
      | Obj (Req (n, t)) -> [ [ n, schema t, true, None ], false ]
      | Obj (Opt (n, t)) -> [ [ n, schema t, false, None ], false ]
      | Obj (Dft (n, t, d)) ->
        let d = Json_repr.repr_to_any (module Json_repr.Ezjsonm) (Ezjsonm_encoding.construct t d) in
        [ [ n, schema t, false, Some d], false ]
      | Objs (o1, o2) ->
        object_schema o1 @@ object_schema o2
      | Union cases ->
        List.fold_left
          (fun acc (Case (o, _, _)) -> object_schema o @@ acc)
          [] cases
      | Mu (_, self) as mu -> object_schema (self mu)
      | Conv (_, _, _, Some _) (* FIXME: We could do better *)
      | _ -> invalid_arg "Json_encoding.schema: consequence of bad merge_objs"
  and array_schema
    : type t. t encoding -> element list
    = function
      | Conv (_, _, o, None) -> array_schema o
      | Tup t -> [ schema t ]
      | Tups (t1, t2) -> array_schema t1 @ array_schema t2
      | Mu (_, self) as mu -> array_schema (self mu)
      | Conv (_, _, _, Some _) (* FIXME: We could do better *)
      | _ -> invalid_arg "Json_encoding.schema: consequence of bad merge_tups"
  and schema
    : type t. t encoding -> element
    = function
      | Null -> element Null
      | Empty -> element (Object { object_specs with additional_properties = None })
      | Ignore -> element Any
      | Int { to_float ; lower_bound ; upper_bound } ->
        let minimum = Some (to_float lower_bound, `Inclusive) in
        let maximum = Some (to_float upper_bound, `Inclusive) in
        element (Integer { multiple_of = None ; minimum ; maximum })
      | Bool -> element Boolean
      | Constant str ->
        { (element (String string_specs)) with
          enum = Some [ Json_repr.to_any (`String str) ] }
      | String -> element (String string_specs)
      | Float (Some { minimum ; maximum }) ->
        element (Number { multiple_of = None ;
                          minimum = Some (minimum, `Inclusive) ;
                          maximum = Some (maximum, `Inclusive) })
      | Float None -> element (Number numeric_specs)
      | Describe (None, None, t) -> schema t
      | Describe (Some _ as title, None, t) ->
        { (schema t) with title }
      | Describe (None, (Some _ as description), t) ->
        { (schema t) with description }
      | Describe (Some _ as title, (Some _ as description), t) ->
        { (schema t) with title ; description }
      | Custom (_, s) ->
        sch := fst (merge_definitions (!sch, s)) ;
        root s
      | Conv (_, _, _, Some s) ->
        sch := fst (merge_definitions (!sch, s)) ;
        root s
      | Conv (_, _, t, None) -> schema t
      | Mu (name, f) ->
        let fake_schema =
          let sch, elt = add_definition name (element Dummy) any in
          update elt sch in
        let fake_self =
          Custom ({ write = (fun _ _ -> assert false) ;
                    read = (fun _ -> assert false) },
                  fake_schema) in
        let nsch, def = add_definition name (schema (f fake_self)) !sch in
        sch := nsch ; def
      | Array t ->
        element (Monomorphic_array (schema t, array_specs))
      | Objs _ as o ->
        begin match object_schema o with
          | [ properties, ext ] ->
            let additional_properties = if ext then Some (element Any) else None in
            element (Object { object_specs with properties ; additional_properties })
          | more ->
            let elements =
              List.map
                (fun (properties, ext) ->
                   let additional_properties = if ext then Some (element Any) else None in
                   element (Object { object_specs with properties ; additional_properties }))
                more in
            element (Combine (One_of, elements))
        end
      | Obj _ as o ->
        begin match object_schema o with
          | [ properties, ext ] ->
            let additional_properties = if ext then Some (element Any) else None in
            element (Object { object_specs with properties ; additional_properties })
          | more ->
            let elements =
              List.map
                (fun (properties, ext) ->
                   let additional_properties = if ext then Some (element Any) else None in
                   element (Object { object_specs with properties ; additional_properties }))
                more in
            element (Combine (One_of, elements))
        end
      | Tup _ as t -> element (Array (array_schema t, array_specs))
      | Tups _ as t -> element (Array (array_schema t, array_specs))
      | Union cases -> (* FIXME: smarter merge *)
        let elements =
          List.map (fun (Case (encoding, _, _)) -> schema encoding) cases in
        element (Combine (One_of, elements)) in
  let schema = schema encoding in
  update schema !sch

(*-- utility wrappers over the GADT ------------------------------------------*)

let req ?title ?description n t = Req (n, Describe (title, description, t))
let opt ?title ?description n t = Opt (n, Describe (title, description, t))
let dft ?title ?description n t d = Dft (n, Describe (title, description, t), d)

let mu name self = Mu (name, self)
let null = Null
let int =
  Int { name = "int" ;
        of_float = int_of_float ;
        to_float = float_of_int ;
        lower_bound = -(1 lsl 30) ;
        upper_bound = (1 lsl 30) - 1 }
let ranged_int ~minimum ~maximum name =
  Int { name ;
        of_float = int_of_float ;
        to_float = float_of_int ;
        lower_bound = minimum ;
        upper_bound = maximum }

let ranged_float ~minimum ~maximum name =
  Float (Some { minimum ;
                maximum })

let float = Float None
let string = String
let conv ffrom fto ?schema t =
  Conv (ffrom, fto, t, schema)
let bytes = Conv (Bytes.to_string, Bytes.of_string, string, None)
let bool = Bool
let array t = Array t
let obj1 f1 = Obj f1
let obj2 f1 f2 = Objs (Obj f1, Obj f2)
let obj3 f1 f2 f3 =
  conv
    (fun (a, b, c) -> (a, (b, c)))
    (fun (a, (b, c)) -> (a, b, c))
    (Objs (Obj f1, Objs (Obj f2, Obj f3)))
let obj4 f1 f2 f3 f4 =
  conv
    (fun (a, b, c, d) -> (a, (b, (c, d))))
    (fun (a, (b, (c, d))) -> (a, b, c, d))
    (Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Obj f4))))
let obj5 f1 f2 f3 f4 f5 =
  conv
    (fun (a, b, c, d, e) -> (a, (b, (c, (d, e)))))
    (fun (a, (b, (c, (d, e)))) -> (a, b, c, d, e))
    (Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Obj f5)))))
let obj6 f1 f2 f3 f4 f5 f6 =
  conv
    (fun (a, b, c, d, e, f) -> (a, (b, (c, (d, (e, f))))))
    (fun (a, (b, (c, (d, (e, f))))) -> (a, b, c, d, e, f))
    (Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, Obj f6))))))
let obj7 f1 f2 f3 f4 f5 f6 f7 =
  conv
    (fun (a, b, c, d, e, f, g) -> (a, (b, (c, (d, (e, (f, g)))))))
    (fun (a, (b, (c, (d, (e, (f, g)))))) -> (a, b, c, d, e, f, g))
    (let rest = Objs (Obj f6, Obj f7) in
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))
let obj8 f1 f2 f3 f4 f5 f6 f7 f8 =
  conv
    (fun (a, b, c, d, e, f, g, h) -> (a, (b, (c, (d, (e, (f, (g, h))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, h))))))) -> (a, b, c, d, e, f, g, h))
    (let rest = Objs (Obj f6, Objs (Obj f7, Obj f8)) in
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))
let obj9 f1 f2 f3 f4 f5 f6 f7 f8 f9 =
  conv
    (fun (a, b, c, d, e, f, g, h, i) -> (a, (b, (c, (d, (e, (f, (g, (h, i)))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, (h, i)))))))) -> (a, b, c, d, e, f, g, h, i))
    (let rest = Objs (Obj f6, Objs (Obj f7, Objs (Obj f8, Obj f9))) in
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))
let obj10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 =
  conv
    (fun (a, b, c, d, e, f, g, h, i, j) -> (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))) -> (a, b, c, d, e, f, g, h, i, j))
    (let rest = Objs (Obj f6, Objs (Obj f7, Objs (Obj f8, Objs (Obj f9, Obj f10)))) in
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))
let tup1 f1 = Tup f1
let tup2 f1 f2 = Tups (Tup f1, Tup f2)
let tup3 f1 f2 f3 =
  conv
    (fun (a, b, c) -> (a, (b, c)))
    (fun (a, (b, c)) -> (a, b, c))
    (Tups (Tup f1, Tups (Tup f2, Tup f3)))
let tup4 f1 f2 f3 f4 =
  conv
    (fun (a, b, c, d) -> (a, (b, (c, d))))
    (fun (a, (b, (c, d))) -> (a, b, c, d))
    (Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tup f4))))
let tup5 f1 f2 f3 f4 f5 =
  conv
    (fun (a, b, c, d, e) -> (a, (b, (c, (d, e)))))
    (fun (a, (b, (c, (d, e)))) -> (a, b, c, d, e))
    (Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tup f5)))))
let tup6 f1 f2 f3 f4 f5 f6 =
  conv
    (fun (a, b, c, d, e, f) -> (a, (b, (c, (d, (e, f))))))
    (fun (a, (b, (c, (d, (e, f))))) -> (a, b, c, d, e, f))
    (Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, Tup f6))))))
let tup7 f1 f2 f3 f4 f5 f6 f7 =
  conv
    (fun (a, b, c, d, e, f, g) -> (a, (b, (c, (d, (e, (f, g)))))))
    (fun (a, (b, (c, (d, (e, (f, g)))))) -> (a, b, c, d, e, f, g))
    (let rest = Tups (Tup f6, Tup f7) in
     Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, rest))))))
let tup8 f1 f2 f3 f4 f5 f6 f7 f8 =
  conv
    (fun (a, b, c, d, e, f, g, h) -> (a, (b, (c, (d, (e, (f, (g, h))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, h))))))) -> (a, b, c, d, e, f, g, h))
    (let rest = Tups (Tup f6, Tups (Tup f7, Tup f8)) in
     Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, rest))))))
let tup9 f1 f2 f3 f4 f5 f6 f7 f8 f9 =
  conv
    (fun (a, b, c, d, e, f, g, h, i) -> (a, (b, (c, (d, (e, (f, (g, (h, i)))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, (h, i)))))))) -> (a, b, c, d, e, f, g, h, i))
    (let rest = Tups (Tup f6, Tups (Tup f7, Tups (Tup f8, Tup f9))) in
     Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, rest))))))
let tup10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 =
  conv
    (fun (a, b, c, d, e, f, g, h, i, j) -> (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))) -> (a, b, c, d, e, f, g, h, i, j))
    (let rest = Tups (Tup f6, Tups (Tup f7, Tups (Tup f8, Tups (Tup f9, Tup f10)))) in
     Tups (Tup f1, Tups (Tup f2, Tups (Tup f3, Tups (Tup f4, Tups (Tup f5, rest))))))

let repr_agnostic_custom { write ; read } ~schema =
  Custom ({ write ; read }, schema)

let describe ?title ?description t = Describe (title, description, t)

let constant s = Constant s

let string_enum cases =
  let schema =
    let specs = Json_schema.({ pattern = None ; min_length = 0 ; max_length = None }) in
    let enum = List.map (fun (s, _) -> Json_repr.(repr_to_any (module Ezjsonm)) (`String s)) cases in
    Json_schema.(update { (element (String specs)) with enum = Some enum } any) in
  let len = List.length cases in
  let mcases = Hashtbl.create len
  and rcases = Hashtbl.create len in
  let cases_str = String.concat " " (List.map (fun x -> "'" ^ fst x ^ "'") cases) in
  List.iter
    (fun (s, c) ->
       if Hashtbl.mem mcases s then
         invalid_arg "Json_encoding.string_enum: duplicate case" ;
       Hashtbl.add mcases s c ;
       Hashtbl.add rcases c s)
    cases ;
  conv
    (fun v -> try Hashtbl.find rcases v with Not_found ->
        invalid_arg (Format.sprintf "Json_encoding.construct: consequence of non exhaustive Json_encoding.string_enum. Strings are: %s" cases_str))
    (fun s ->
       (try Hashtbl.find mcases s with Not_found ->
          let rec orpat ppf = function
            | [] -> assert false
            | [ last, _ ] -> Format.fprintf ppf "%S" last
            | [ prev, _ ; last, _ ] -> Format.fprintf ppf "%S or %S" prev last
            | (prev, _) :: rem -> Format.fprintf ppf "%S , %a" prev orpat rem in
          let unexpected = Format.asprintf "string value %S" s in
          let expected = Format.asprintf "%a" orpat cases in
          raise (Cannot_destruct ([], Unexpected (unexpected, expected)))))
    ~schema
    string

let def name encoding =
  let schema =
    let open Json_schema in
    let sch = schema encoding in
    let sch, def = add_definition name (root sch) sch in
    update def sch in
  conv (fun v -> v) (fun v -> v) ~schema encoding

let assoc : type t. t encoding -> (string * t) list encoding = fun t ->
  Ezjsonm_encoding.custom
    (fun l -> `O (List.map (fun (n, v) -> n, Ezjsonm_encoding.construct t v) l))
    (fun v -> match v with
       | `O l ->
         let destruct n t v = try
             Ezjsonm_encoding.destruct t v
           with Cannot_destruct (p, exn) -> raise (Cannot_destruct (`Field n :: p, exn)) in
         List.map (fun (n, v) -> n, destruct n t v) l
       | #Json_repr.ezjsonm as k -> raise (unexpected k "asssociative object"))
    (let s = schema t in
     Json_schema.(update (element (Object { object_specs with additional_properties = Some (root s)})) s))

let option : type t. t encoding -> t option encoding = fun t ->
  let read
    : type tf. (module Json_repr.Repr with type value = tf) -> tf -> t option
    = fun (module Repr_f) repr ->
      match Repr_f.view repr with
      | `Null -> None
      | _ ->
        let module Repr_f_encoding = Make (Repr_f) in
        Some (Repr_f_encoding.destruct t repr) in
  let write
    : type tf. (module Json_repr.Repr with type value = tf) -> t option -> tf
    = fun (module Repr_f) v ->
      match v with
      | None -> Repr_f.repr `Null
      | Some v ->
        let module Repr_f_encoding = Make (Repr_f) in
        Repr_f_encoding.construct t v in
  let schema =
    let s = schema t in
    Json_schema.(update (element (Combine (One_of, [(root s) ; element Null]))) s) in
  Custom ({ read ; write }, schema)

let int32 =
  Int { name = "int32" ;
        of_float = Int32.of_float ;
        to_float = Int32.to_float ;
        lower_bound = Int32.min_int ;
        upper_bound = Int32.max_int }

let any_value =
  let read repr v = Json_repr.repr_to_any repr v in
  let write repr v = Json_repr.any_to_repr repr v in
  Custom ({ read ; write }, Json_schema.any)

let any_ezjson_value =
  let read repr v = Json_repr.convert repr (module Json_repr.Ezjsonm) v in
  let write repr v = Json_repr.convert (module Json_repr.Ezjsonm) repr v in
  Custom ({ read ; write }, Json_schema.any)

let any_document =
  let read
    : type tt. (module Json_repr.Repr with type value = tt) -> tt -> Json_repr.any
    = fun (module Repr) v ->
      match Repr.view v with
      | `A _ | `O _ ->
        Json_repr.repr_to_any (module Repr) v
      | k -> raise @@ unexpected k "array or object" in
  let write repr v = Json_repr.any_to_repr repr v in
  Custom ({ read ; write }, Json_schema.any)

let any_schema =
  Ezjsonm_encoding.custom
    Json_schema.to_json
    (fun j -> try Json_schema.of_json j with err ->
        raise (Cannot_destruct ([], Bad_schema err)))
    Json_schema.self

let merge_tups t1 t2 =
  let rec is_tup : type t. t encoding -> bool = function
    | Tup _ -> true
    | Tups _ (* by construction *) -> true
    | Conv (_, _, t, None) -> is_tup t
    | Mu (_name, self) as mu -> is_tup (self mu)
    | _ -> false in
  if is_tup t1 && is_tup t2 then
    Tups (t1, t2)
  else
    invalid_arg "Json_encoding.merge_tups"

let list t =
  Conv (Array.of_list, Array.to_list, Array t, None)

let merge_objs o1 o2 =
  (* FIXME: check fields unicity *)
  let rec is_obj : type t. t encoding -> bool = function
    | Obj _ -> true
    | Objs _ (* by construction *) -> true
    | Conv (_, _, t, None) -> is_obj t
    | Empty -> true
    | Ignore -> true
    | Union cases -> List.for_all (fun (Case (o, _, _)) -> is_obj o) cases
    | Mu (_name, self) as mu -> is_obj (self mu)
    | _ -> false in
  if is_obj o1 && is_obj o2 then
    Objs (o1, o2)
  else
    invalid_arg "Json_encoding.merge_objs"

let empty =
  Empty

let unit =
  Ignore

let case encoding fto ffrom =
  Case (encoding, fto, ffrom)

let union = function
  | [] -> invalid_arg "Json_encoding.union"
  | cases ->
    (* FIXME: check mutual exclusion *)
    Union cases

let rec print_error ?print_unknown ppf = function
  | Cannot_destruct ([], exn) ->
    print_error ?print_unknown ppf exn
  | Cannot_destruct (path, Unexpected (unex, ex)) ->
    Format.fprintf ppf
      "At %a, unexpected %s instead of %s"
      (Json_query.print_path_as_json_path ~wildcards:true) path
      unex ex
  | Cannot_destruct (path, No_case_matched errs) ->
    Format.fprintf ppf
      "@[<v 2>At %a, no case matched:@,%a@]"
      (Json_query.print_path_as_json_path ~wildcards:true) path
      (Format.pp_print_list (print_error ?print_unknown)) errs
  | Cannot_destruct (path, Bad_array_size (unex, ex)) ->
    Format.fprintf ppf
      "At %a, unexpected array of size %d instead of %d"
      (Json_query.print_path_as_json_path ~wildcards:true) path
      unex ex
  | Cannot_destruct (path, Missing_field n) ->
    Format.fprintf ppf
      "At %a, missing object field %s"
      (Json_query.print_path_as_json_path ~wildcards:true) path
      n
  | Cannot_destruct (path, Unexpected_field n) ->
    Format.fprintf ppf
      "At %a, unexpected object field %s"
      (Json_query.print_path_as_json_path ~wildcards:true) path
      n
  | Cannot_destruct (path, Bad_schema exn) ->
    Format.fprintf ppf
      "@[<v 2>At %a, bad custom schema:@,%a@]"
      (Json_query.print_path_as_json_path ~wildcards:true) path
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
      (Json_query.print_path_as_json_path ~wildcards:true) path
      (print_error ?print_unknown) exn
  | exn ->
    Json_schema.print_error ?print_unknown ppf exn

include Ezjsonm_encoding
