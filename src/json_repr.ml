(* Representations of JSON documents *)

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

type 'a view =
  [ `O of (string * 'a) list
  | `A of 'a list
  | `Bool of bool
  | `Float of float
  | `String of string
  | `Null ]

module type Repr = sig
  type value
  val view : value -> value view
  val repr : value view -> value
end

module Ezjsonm = struct
  type value =
    [ `O of (string * value) list
    | `A of value list
    | `Bool of bool
    | `Float of float
    | `String of string
    | `Null ]
  let view v = v
  let repr v = v
end

type ezjsonm = Ezjsonm.value

module Yojson = struct
  type value =
    [ `Bool of bool
    | `Assoc of (string * value) list
    | `Float of float
    | `Int of int
    | `Intlit of string
    | `List of value list
    | `Null
    | `String of string
    | `Tuple of value list
    | `Variant of string * value option ]
  let view = function
    | `Intlit i -> `String i
    | `Tuple l -> `A l
    | `Variant (label, Some x) -> `A [ `String label ; x ]
    | `Variant (label, None) -> `String label
    | `Assoc l -> `O l
    | `List l -> `A l
    | `Int i -> `Float (float i)
    | `Float f -> `Float f
    | `String s -> `String s
    | `Null -> `Null
    | `Bool b -> `Bool b
  let repr = function
    | `O l -> `Assoc l
    | `A l -> `List l
    | `Bool b -> `Bool b
    | `Float f -> `Float f
    | `String s -> `String s
    | `Null -> `Null
end
type yojson = Yojson.value

let convert
  : type tt tf.
    (module Repr with type value = tf) ->
    (module Repr with type value = tt) ->
    tf -> tt
  = fun (module Repr_f) (module Repr_t) v ->
    let rec conv v = match Repr_f.view v with
      | `Float _ | `Bool _ | `String _ | `Null as v -> Repr_t.repr v
      | `A values -> Repr_t.repr (`A (List.map conv values))
      | `O values -> Repr_t.repr (`O (List.map (fun (k, v) -> (k, conv v)) values)) in
    conv v

let from_yojson non_basic =
  (* Delete `Variant, `Tuple and `Intlit *)
  let rec to_basic non_basic = match non_basic with
    | `Intlit i -> `String i
    | `Tuple l -> `List (List.map to_basic l)
    | `Variant (label, Some x) -> `List [`String label; to_basic x]
    | `Variant (label, None) -> `String label
    | `Assoc l -> `Assoc (List.map (fun (key, value) -> (key, to_basic value)) l)
    | `List l -> `List (List.map to_basic l)
    | `Int i -> `Int i
    | `Float f -> `Float f
    | `String s -> `String s
    | `Null -> `Null
    | `Bool b -> `Bool b in
  (* Rename `Assoc, `Int and `List *)
  let rec to_value : 'a. _ -> ([> ezjsonm ] as 'a) = function
    | `List l -> `A (List.map to_value l)
    | `Assoc l -> `O (List.map (fun (key, value) -> (key, to_value value)) l)
    | `Int i -> `Float (float_of_int i)
    | `Float f -> `Float f
    | `Null -> `Null
    | `String s -> `String s
    | `Bool b -> `Bool b in
  to_basic (non_basic :> yojson) |> to_value

let rec to_yojson json =
  let rec aux : 'a. _ -> ([> yojson ] as 'a) = function
    | `A values ->
      `List (List.map aux values)
    | `O values ->
      `Assoc (List.map (fun (k, v) -> (k, aux v)) values)
    | `Float f ->
      let (fract, intr) = modf f in
      let (min_intf, max_intf) = (min_int |> float_of_int,
                                  max_int |> float_of_int) in
      if fract = 0.0 then
        if intr >= min_intf && intr <= max_intf
        then `Int (int_of_float intr)
        else `Intlit (Printf.sprintf "%.0f" intr)
      else `Float f
    | `Bool b -> `Bool b
    | `String s -> `String s
    | `Null -> `Null
  in aux (json :> ezjsonm)

type any = Value_with_repr: (module Repr with type value = 'a) * 'a -> any

let any_to_repr :
  type tt. (module Repr with type value = tt) -> any -> tt =
  fun repr_t (Value_with_repr (repr_f, v)) -> convert repr_f repr_t v

let repr_to_any repr v =
  Value_with_repr (repr, v)

let from_any : 'a. any -> ([> ezjsonm] as 'a) = fun repr ->
  let res = any_to_repr (module Ezjsonm) repr in
  (res : ezjsonm :> [> ezjsonm])

let to_any v =
  Value_with_repr ((module Ezjsonm), (v :> ezjsonm))
