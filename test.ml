
let to_channel chan json =
  Ezjsonm.to_channel chan (json :> Ezjsonm.t)

let () =
  let open Json_typed in
  let json = Ezjsonm.from_channel stdin in
  let v =
    destruct
      (union
         [ case (tup1 int)
             (function (n, None) -> Some n | _ -> None)
             (fun n -> (n, None)) ;
           case (tup2 int int)
             (function (n1, Some n2) -> Some (n1, n2) | _ -> None)
             (fun (n1, n2) -> (n1, Some n2)) ])
      json in
  let json =
    construct
      (obj2
         (req "paté" int)
         (opt "saucisse" int))
      v in
  to_channel stdout json
