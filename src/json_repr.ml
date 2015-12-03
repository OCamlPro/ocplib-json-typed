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

(*-- types and errors --------------------------------------------------------*)

type document =
  [ `O of (string * value) list
  | `A of value list ]

and value =
  [ `O of (string * value) list
  | `A of value list
  | `Bool of bool
  | `Float of float
  | `String of string
  | `Null ]

type yojson =
  [ `Bool of bool
  | `Assoc of (string * yojson) list
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of yojson list
  | `Null
  | `String of string
  | `Tuple of yojson list
  | `Variant of string * yojson option ]

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
  let rec to_value : 'a. _ -> ([> value ] as 'a) = function
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
  in aux (json :> value)

type path =
  path_item list

and path_item =
  [ `Field of string
  | `Index of int
  | `Star | `Next ]

exception Illegal_pointer_notation of string * int * string
exception Unsupported_path_item of path_item * string
exception Cannot_merge of path

(*-- path operations ---------------------------------------------------------*)

let print_path_as_json_path ?(wildcards = true) ppf = function
  | [] -> Format.fprintf ppf "/"
  | nonempty ->
    let rec print ppf = function
      | [] -> ()
      | `Field n :: rem -> Format.fprintf ppf "/%s%a" n print rem
      | `Index n :: rem -> Format.fprintf ppf "[%d]%a" n print rem
      | `Next :: rem when wildcards -> Format.fprintf ppf "-%a" print rem
      | `Star :: rem when wildcards -> Format.fprintf ppf "*%a" print rem
      | (`Next | `Star) :: _ ->
        raise (Unsupported_path_item (`Star, "JSON path w/o wildcards")) in
    print ppf nonempty

let print_path_as_json_pointer ?(wildcards = true) ppf = function
  | [] -> Format.fprintf ppf "/"
  | nonempty ->
    let rec print ppf = function
      | [] -> ()
      | `Field n :: rem -> Format.fprintf ppf "/%s%a" n print rem
      | `Index n :: rem -> Format.fprintf ppf "/%d%a" n print rem
      | `Next :: rem when wildcards -> Format.fprintf ppf "/-%a" print rem
      | `Next :: _ -> raise (Unsupported_path_item (`Star, "JSON pointer w/o wildcards"))
      | `Star :: _ -> raise (Unsupported_path_item (`Star, "JSON pointer")) in
    print ppf nonempty

let json_pointer_of_path ?wildcards path =
  Format.asprintf "%a" (print_path_as_json_pointer ?wildcards) path

let path_of_json_pointer ?(wildcards = true) str =
  let buf = Buffer.create 100 in
  let len = String.length str in
  let rec slashes acc i =
    if i >= len then List.rev acc
    else if String.get str i = '/' then slashes acc (i + 1)
    else item acc i
  and item acc i =
    if i >= len then List.rev (interp () :: acc)
    else match String.get str i with
      | '/' -> slashes (interp () :: acc) i
      | '~' ->
        if i + 1 >= len then
          raise (Illegal_pointer_notation (str, i, "Unterminated escape sequence")) ;
        begin match String.get str i with
          | '0' -> Buffer.add_char buf '~'
          | '1' -> Buffer.add_char buf '/'
          | _illegal ->
            raise (Illegal_pointer_notation (str, i + 1, "Illegal escape character")) end ;
        item acc (i + 1)
      | unescaped ->
        Buffer.add_char buf unescaped ;
        item acc (i + 1)
  and interp () =
    let field = Buffer.contents buf in
    Buffer.clear buf ;
    if field = "-" then
      if wildcards then
        `Next
      else
        raise (Unsupported_path_item (`Next, "JSON pointer w/o wildcards"))
    else try `Index (int_of_string field) with
      | _ -> `Field field in
  if len = 0 then []
  else if String.get str 0 <> '/' then
    raise (Illegal_pointer_notation (str, 0, "Missing initial slash"))
  else slashes [] 1

(*-- queries -----------------------------------------------------------------*)

let rec query
  : 'a. path -> ([< value ] as 'a) -> value
  = fun path json -> match path, json with
    | [], json ->
      (json :> value)
    | `Field n :: rempath, `O ((n', v) :: rem) ->
      if n = n' then query rempath v else query path (`O rem)
    | `Index i :: rempath, `A cells ->
      let i = if i < 0 then List.length cells - i  else i in
      query rempath (List.nth cells i)
    | `Star :: rempath, `O ((n, v) :: rem) ->
      begin try query rempath v with Not_found -> query path (`O rem) end
    | `Star :: rempath, `A (v :: rem) ->
      begin try query rempath v with Not_found -> query path (`A rem) end
    | _, _ -> raise Not_found

let query_all path json =
  let res = ref [] in
  let rec query
    : 'a. path -> ([< value ] as 'a) -> unit
    = fun path json -> match path, json with
      | [], json ->
        res := (json :> value) :: !res
      | `Field n :: rempath, `O ((n', v) :: rem) ->
        if n = n' then query rempath v else query path (`O rem)
      | `Index i :: rempath, `A cells ->
        let i = if i < 0 then List.length cells - i  else i in
        query rempath (List.nth cells i)
      | `Star :: rempath, `O fields ->
        List.iter (fun (_, v) -> query rempath v) fields
      | `Star :: rempath, `A cells ->
        List.iter (query rempath) cells
      | _, _ -> () in
  query path json ; !res

(*-- updates -----------------------------------------------------------------*)

let sort_fields =
  List.sort (fun (l, _) (r, _) -> compare l r)

let rec canon = function
  | `O l -> `O (List.map (fun (n, o) -> n, canon o) l |> sort_fields)
  | `A l -> `A (List.map canon l)
  | imm -> imm

let equals l r =
  canon l = canon r

let merge l r =
  let rec merge path l r =
    match l, r with
    | `O l, `O r -> `O (merge_fields path [] (sort_fields (l @ r)))
    | `Null, v | v, `Null -> v
    | `A l, `A r -> `A (merge_cells path 0 [] l r)
    | vl, vr when equals vl vr -> vl
    | _ -> raise (Cannot_merge (List.rev path))
  and merge_cells path i acc l r = match l, r with
    | [], rem | rem, [] -> List.rev_append acc rem
    | l :: ls, r :: rs ->
      let item = merge (`Index i :: path) l r in
      merge_cells path (succ i) (item :: acc) ls rs
  and merge_fields path acc = function
    | (lf, lv) :: ((rf, rv) :: rem as rrem) ->
      if lf = rf then
        let item = merge (`Field lf :: path) lv rv in
        merge_fields path ((lf, item) :: acc) rem
      else
        merge_fields path ((lf, lv) :: acc) rrem
    | [ _ ] | [] as last -> last in
  merge [] (l :> value) (r :> value)

let insert ?(merge = merge) path value root =
  let revpath sub =
    let rec loop acc = function
      | l when l == sub -> List.rev acc
      | item :: items -> loop (item :: acc) items
      | [] -> (* absurd *) assert false
    in loop [] path in
  let merge path l r =
    try merge l r with
      Cannot_merge sub -> raise (Cannot_merge (revpath path @ sub)) in
  let value = (value :> value) in
  let root = (root :> value) in
  let rec nulls acc n last =
    if n <= 0 then
      List.rev (last :: acc)
    else
      nulls (`Null :: acc) (pred n) last in
  let rec insert ?root path = match path, root with
    (* create objects *)
    | `Field n :: rempath, None ->
      `O [ (n, insert rempath) ]
    | (`Index 0  | `Star | `Next) :: rempath, None ->
      `A [ insert rempath ]
    | `Index i :: rempath, None ->
      if i < 0 then raise (Cannot_merge (revpath path)) ;
      `A (nulls [] (max 0 (pred i)) (insert rempath))
    | [], None -> value
    (* insert in existing *)
    | [], Some value' ->
      merge path value value'
    | `Field n :: rempath, Some (`O fields) ->
      `O (insert_fields [] n rempath fields)
    | `Index i :: rempath, Some (`A cells) ->
      let i = if i < 0 then List.length cells - i else i in
      if i < 0 then raise (Cannot_merge (revpath path)) ;
      `A (insert_cells [] i rempath cells)
    | `Next :: rempath, Some (`A cells) ->
      `A (List.rev_append (List.rev cells) [ insert rempath ])
    (* multiple insertions *)
    | `Star :: rempath, Some (`A cells) ->
      `A (List.map (fun root -> insert ~root rempath) cells)
    | `Star :: rempath, Some (`O fields) ->
      `O (List.map (fun (n, root) -> (n, insert ~root rempath)) fields)
    | [ `Star ], Some root ->
      merge path value root
    (* FIXME: make explicit unhandled cases *)
    | _, Some _ -> raise (Cannot_merge (revpath path))
  and insert_fields acc n rempath fields = match fields with
    | [] ->
      List.rev ((n, insert rempath) :: acc)
    | (n', root) :: rem when n = n' ->
      List.rev_append ((n, insert ~root rempath) :: acc) rem
    | other :: rem ->
      insert_fields (other :: acc) n rempath rem
  and insert_cells acc n rempath cells =
    match cells, n with
    | [], n ->
      nulls acc n (insert rempath)
    | root :: rem, 0 ->
      List.rev_append ((insert ~root rempath) :: acc) rem
    | other :: rem, n ->
      insert_cells (other :: acc) (n - 1) rempath rem in
  insert ~root path

let replace path value root =
  insert ~merge:(fun value _prev -> value) path value root

let insert path value root =
  insert path value root

let path_operator_name = function
  | `Field _ -> "field access"
  | `Index _ -> "array access"
  | `Star -> "wildcard"
  | `Next -> "array append"

let rec print_error ?print_unknown ppf err = match err with
  | Illegal_pointer_notation (notation, pos, msg) ->
    Format.fprintf ppf
      "@[<v 2>Illegal pointer notation@,At character %d of %S@,%s@]"
      pos notation msg
  | Unsupported_path_item (item, msg) ->
    Format.fprintf ppf
      "Path operator %s unsupported by %s"
      (path_operator_name item) msg
  | Cannot_merge [] ->
    Format.fprintf ppf
      "Unmergeable objects"
  | Cannot_merge path ->
    Format.fprintf ppf
      "Unmergeable objects, incompatibility at %a"
      (print_path_as_json_path ~wildcards:true) path
  | exn ->
    match print_unknown with
    | Some print_unknown -> print_unknown ppf exn
    | None ->
      Format.fprintf ppf "Unhandled error %s" (Printexc.to_string exn)
