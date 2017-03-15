module Decoder = struct 

  type t = {
    dict : Js_json.t Js_dict.t;
    mutable index : int; 
  }

  type value = 
    | String of string 
    | Float of float 
    | Int of int 
    | Object of t 
    | Array_as_array of value array 
    | Bool of bool 
    | Null

  let of_json json = 
    match Js_json.reify_type json with
    | Js_json.Object, dict ->
      Some ({dict; index = 0}) 
    | _ -> None

  let rec map : type a. a Js_json.kind -> a -> value = fun ty x -> 
    match ty with
    | Js_json.String -> String x
    | Js_json.Number -> Float x
    | Js_json.Boolean -> Bool (Js.to_bool x)
    | Js_json.Null -> Null
    | Js_json.Object -> Object {dict = x; index = 0} 
    | Js_json.Array -> Array_as_array (Array.map (fun e -> 
        let ty, x = Js_json.reify_type e in 
        map ty x
    ) x)

  let key t = 
    let keys = Js_dict.keys t.dict in 
    if t.index >= (Array.length keys) 
    then None
    else 
      let key = keys.(t.index) in 
      let value = 
        match Js_dict.get t.dict key with 
        | None -> assert(false) 
        | Some json -> 
          let ty, x = Js_json.reify_type json in
          map ty x 
      in
      t.index <- t.index + 1;
      Some (key, value)
end 

let () = 
  let decoder = 
    Js_json.parse "{\"a\" : \"b\"}" 
    |> Decoder.of_json
  in
  match decoder with
  | None -> print_endline "Error creating decoder"
  | Some decoder -> 
    print_endline "Decoder created";

    begin match Decoder.key decoder with
    | Some ("a", Decoder.String "b") -> () 
    | _ -> assert(false) 
    end; 
    begin match Decoder.key decoder with
    | None -> () 
    | _ -> assert(false)
    end;
    ()
