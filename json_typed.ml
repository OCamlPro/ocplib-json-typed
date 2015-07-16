
type document =
  [ `O of (string * value) list | `A of value list ]
and immediate =
  [ `Bool of bool | `Float of float | `String of string | `Null ]
and value =
  [ `O of (string * value) list | `A of value list
  | `Bool of bool | `Float of float | `String of string | `Null ]

type path = path_item list
and path_item =
  [ `Field of string | `Index of int | `Star ]

exception Illegal_pointer_notation of string

exception Cannot_destruct of path * string

let unexpected k =
  raise (Cannot_destruct ([], match k with
    | `O _ -> "object"
    | `A _ -> "array"
    | `Null -> "null"
    | `String _ -> "string"
    | `Float _ -> "number"
    | `Bool _ -> "boolean"))

type (_, 'k) cotcodec =
  | Null : (unit, [ `Null ]) cotcodec
  | Int : (int, [ `Float of float ]) cotcodec
  | Bool : (bool, [ `Bool of bool ]) cotcodec
  | String : (string, [ `String of string ]) cotcodec
  | Float : (float, [ `Float of float ]) cotcodec
  | Array : ('a, [< value ]) cotcodec -> ('a array, [ `A of value list ]) cotcodec
  | Obj : 'a field -> ('a, [ `O of (string * value) list ]) cotcodec
  | Objs :
      ('a, [ `O of (string * value) list ]) cotcodec *
      ('b, [ `O of (string * value) list ]) cotcodec ->
    ('a * 'b, [ `O of (string * value) list ]) cotcodec
  | Tup : ('a, [< value ]) cotcodec -> ('a, [ `A of value list ]) cotcodec
  | Tups :
      ('a, [ `A of value list ]) cotcodec *
      ('b, [ `A of value list ]) cotcodec ->
    ('a * 'b, [ `A of value list ]) cotcodec
  | Custom : 'k witness * ('t -> 'k) * (value -> 't) * Json_schema.schema -> ('t, 'k) cotcodec
  | Conv : ('a -> 'b) * ('b -> 'a) * ('b, 'k) cotcodec -> ('a, 'k) cotcodec
  | Describe : string option * string option * ('a, 'k) cotcodec -> ('a, 'k) cotcodec
  | Mu : string * (('a, 'k) cotcodec -> ('a, 'k) cotcodec) -> ('a, 'k) cotcodec

and _ field =
  | Req : string * ('a, [< value ]) cotcodec -> 'a field
  | Opt : string * ('a, [< value ]) cotcodec -> 'a option field

and _ witness =
  | Document_witness : document witness
  | Value_witness : value witness
  | Obj_witness : [ `O of (string * value) list ] witness
  | Tup_witness : [ `A of value list ] witness
  | Custom_witness : (_, 'k) cotcodec -> 'k witness

let x = Array Int

type ('a, 'b) codec = ('a, 'b) cotcodec constraint 'b = [< value]

let rec construct
  : type t k. (t, k) cotcodec -> (t -> k)
  = function
    | Null -> (fun () -> `Null)
    | Int -> (fun (i : t) -> `Float (float  i))
    | Bool -> (fun (b : t) -> `Bool b)
    | String -> (fun s -> `String s)
    | Float -> (fun f -> `Float f)
    | Describe (_, _, t) -> construct t
    | Custom (_, w, _, _) -> (fun (j : t) -> (w j : k))
    | Conv (ffrom, _, t) -> (fun v -> construct t (ffrom v))
    | Mu (name, self) -> construct (self (Mu (name, self)))
    | Array t ->
      let w v = (construct t v :> value) in
      (fun arr -> `A (Array.to_list (Array.map w arr)))
    | Obj (Req (n, t)) ->
      let w v = (construct t v :> value) in
      (fun v -> `O [ n, w v ])
    | Obj (Opt (n, t)) ->
      let w v = (construct t v :> value) in
      (function None -> `O [] | Some v -> `O [ n, w v ])
    | Objs (o1, o2) ->
      let w1 v = construct o1 v in
      let w2 v = construct o2 v in
      (function (v1, v2) -> let `O l1, `O l2 = w1 v1, w2 v2 in `O (l1 @ l2))
    | Tup t ->
      let w v = (construct t v :> value) in
      (fun v -> `A [ w v ])
    | Tups (o1, o2) ->
      let w1 v = construct o1 v in
      let w2 v = construct o2 v in
      (function (v1, v2) -> let `A l1, `A l2 = w1 v1, w2 v2 in `A (l1 @ l2))

let rec destruct
  : type t k. (t, k) cotcodec -> (value -> t)
  = function
    | Null -> (function `Null -> () | k -> unexpected k)
    | Int -> (function `Float f -> int_of_float f | k -> unexpected k)
    | Bool -> (function `Bool b -> (b : t) | k -> unexpected k)
    | String -> (function `String s -> s | k -> unexpected k)
    | Float -> (function `Float f -> f | k -> unexpected k)
    | Describe (_, _, t) -> destruct t
    | Custom (_, _, r, _) -> r
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
        | k -> unexpected k)
    | Obj (Req (n, t)) ->
      (function
        | `O fields ->
          (try destruct t (List.assoc n fields) with
           | Not_found ->
             raise (Cannot_destruct ([ `Field n ], "missing field"))
           | Cannot_destruct (path, err) ->
             raise (Cannot_destruct (`Field n :: path, err)))
        | k -> unexpected k)
    | Obj (Opt (n, t)) ->
      (function
        | `O fields ->
          (try Some (destruct t (List.assoc n fields)) with
           | Not_found -> None
           | Cannot_destruct (path, err) ->
             raise (Cannot_destruct (`Field n :: path, err)))
        | k -> unexpected k)
    | Objs (o1, o2) ->
      (fun j -> destruct o1 j, destruct o2 j)
    | Tup _ as t ->
      let r, i = destruct_tup 0 (t :> (t, [ `A of value list]) cotcodec) in
      (function
        | `A cells ->
          let cells = Array.of_list cells in
          if i > Array.length cells then
            raise (Cannot_destruct ([], "array too small"))
          else if i < Array.length cells then
            raise (Cannot_destruct ([], "array too big"))
          else r cells
        | k -> unexpected k)
    | Tups _ as t ->
      let r, i = destruct_tup 0 t in
      (function
        | `A cells ->
          let cells = Array.of_list cells in
          if i > Array.length cells then
            raise (Cannot_destruct ([], "array too big"))
          else if i < Array.length cells then
            raise (Cannot_destruct ([], "array too small"))
          else r cells
        | k -> unexpected k)
and destruct_tup
  : type t. int -> (t, [ `A of value list ]) cotcodec -> (value array -> t) * int
  = fun i t -> match t with
    | Tup t ->
      (fun arr -> destruct t arr.(i)), succ i
    | Tups (t1, t2) ->
      let r1, i = destruct_tup i t1 in
      let r2, i = destruct_tup i t2 in
      (fun arr -> r1 arr, r2 arr), i
    | _ ->
      invalid_arg "Json_typed.destruct_tup"

let destruct t =
  let d = destruct t in
  fun j -> d (j :> value)

let schema _ = Json_schema.any

let req n t = Req (n, t)
let opt n t = Opt (n, t)

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
    Custom (Custom_witness t, ffrom, fto, schema)
let bytes = Conv (Bytes.of_string, Bytes.to_string, string)
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



let custom w r ~schema = Custom (Value_witness, w, r, schema)
let any = custom (fun value -> value) (fun value -> (value :> value)) Json_schema.any
let describe ?title ?description t = Describe (title, description, t)
let string_enum cases =
  let specs = Json_schema.({ pattern = None ; min_length = 0 ; max_length = None }) in
  let enum = List.map (fun (s, _) -> `String s) cases in
  let rcases = List.map (fun (s, c) -> (c, s)) cases in
  conv
    (fun v -> try List.assoc v rcases with Not_found -> invalid_arg "Json_typed.string_enum")
    (fun s ->
       (try List.assoc s cases with Not_found ->
         raise (Cannot_destruct ([], "unexpected string"))))
    ~schema: Json_schema.(update { (element (String specs)) with enum = Some enum } any)
    string

let json_schema =
  Json_schema.(custom to_json
                 (fun j -> match of_json j with
                    | Some v -> v
                    | None -> raise (Cannot_destruct ([], "invalid inline JSON schema"))) self)

let def name cotcodec =
  Custom
    (Custom_witness cotcodec,
     construct cotcodec,
     destruct cotcodec,
     (let open Json_schema in
      let sch = schema cotcodec in
      let sch, def = add_definition [ name ] sch.root sch in
      update def sch))

let option : type t k. (t, [< value ]) cotcodec -> (t option, value) cotcodec = fun t ->
  Custom
    (Value_witness,
     (function None -> `Null | Some v -> (construct t v :> value)),
     (function `Null -> None | j -> Some (destruct t j)),
     let s = schema t in
     Json_schema.(update (element (Combine (One_of, [s.root ; element Null]))) s))

let int32 =
  Conv (Int32.to_float, Int32.of_float, float)


let any_value =
  Custom
    (Value_witness,
     (fun value -> value),
     (fun value -> value),
     Json_schema.any)

let any_document =
  Custom
    (Document_witness,
     (fun value -> value),
     (function `A _ | `O _ as d -> d | k -> unexpected k),
     Json_schema.any)

let any_schema =
  Custom
    (Value_witness,
     Json_schema.to_json,
     (fun j -> match Json_schema.of_json j with
        | None -> raise (Cannot_destruct ([], "bad schema"))
        | Some schema -> schema),
     Json_schema.self)

let merge_tups t1 t2 =
  Tups (t1, t2)
let list t =
  Conv (Array.of_list, Array.to_list, Array t)
let merge_objs o1 o2 =
  Objs (o1, o2)
let empty =
  Custom
    (Obj_witness,
     (fun () -> `O []),
     (function
       | `O [] -> ()
       | `O _ -> raise (Cannot_destruct ([], "non empty object"))
       | k -> unexpected k),
     Json_schema.(create (element (Object { properties = [] ;
                                            pattern_properties = [] ;
                                            additional_properties = None ;
                                            min_properties = 0 ;
                                            max_properties = Some 0 ;
                                            schema_dependencies = [] ;
                                            property_dependencies = [] }))))

let unit =
  Custom
    (Value_witness,
     (fun () -> `O []),
     (function
       | _ -> ()),
     Json_schema.any)

type 't case = Case : ('a, 'k) codec * ('t -> 'a option) * ('a -> 't) -> 't case

let case codec fto ffrom =
  Case (codec, fto, ffrom)

let union = function
  | [] -> invalid_arg "Json_typed.union"
  | l ->
    Custom
      (Value_witness,
       (fun v ->
          let rec do_cases = function
            | [] -> invalid_arg "Json_typed.union"
            | Case (codec, fto, _) :: rest ->
              match fto v with
              | Some v -> (construct codec v :> value)
              | None -> do_cases rest in
          do_cases l),
       (fun v ->
          let rec do_cases = function
            | [] -> raise (Cannot_destruct ([], "no case matched"))
            | Case (codec, _, ffrom) :: rest ->
              try ffrom (destruct codec v) with
                Cannot_destruct _ -> do_cases rest in
          do_cases l),
       Json_schema.combine
         Json_schema.One_of
         (List.map (fun (Case (codec, _, _)) -> schema codec) l))
