
let to_channel chan json =
  Ezjsonm.to_channel ~minify:false chan (json :> Ezjsonm.t) ;
  Printf.fprintf stdout "\n%!"

let () =
  let open Json_typed in
  let input_f itemcodec =
    obj1 (req "contents" @@
          mu "list" @@ fun self ->
          describe
            ~title: "a simple linked list of integers" @@
          union
            [ case (string_enum [ "nil", () ])
                (function [] -> Some () | _ :: _ -> None)
                (fun () -> []) ;
              case (obj2 (req "hd" itemcodec) (req "tl" self))
                (function hd :: tl -> Some (hd, tl) | [] -> None)
                (fun (hd, tl) -> hd :: tl) ]) in
  let output_f =
    obj1 (req "contents" (list int)) in
  to_channel stdout (Json_schema.to_json (schema (input_f int))) ;
  to_channel stdout (Json_schema.to_json (schema output_f)) ;
  let rec main () =
    try
      let json = Ezjsonm.from_channel stdin in
      let v = destruct (input_f int) json in
      let json = construct output_f v in
      to_channel stdout json
    with err ->
      let print_unknown ppf = function
        | Ezjsonm.Parse_error (_, err) ->
          Format.fprintf ppf "%s" err
        | exn -> raise exn in
      Format.eprintf "%a@." (print_error ~print_unknown) err ;
      main () in
  main ()

(*
let () =
  let open Json_typed in
  let input_f =
    (obj1
       (req "P"
          (union
             [ case (obj1 (req "NoY" (tup1 int)))
                 (function (n, None) -> Some n | _ -> None)
                 (fun n -> (n, None)) ;
               case (tup2 int int)
                 (function (n1, Some n2) -> Some (n1, n2) | _ -> None)
                 (fun (n1, n2) -> (n1, Some n2)) ]))) in
  let output_f =
    (obj2
       (req "X" int)
       (opt "Y" int)) in
  to_channel stdout (Json_schema.to_json (schema input_f)) ;
  to_channel stdout (Json_schema.to_json (schema output_f)) ;
  let rec main () =
    try
      let json = Ezjsonm.from_channel stdin in
      let v = destruct input_f json in
      let json = construct output_f v in
      to_channel stdout json
    with
    | Cannot_destruct err ->
      Format.eprintf "%a@." print_error err ;
      main ()
    | Ezjsonm.Parse_error (_, err) ->
      Format.eprintf "%s@." err ;
      main () in
  main ()
*)
