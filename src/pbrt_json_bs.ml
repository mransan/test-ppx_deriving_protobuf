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
    
  let of_string s = 
    s |> Js_json.parse |> of_json

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
        | None -> failwith "we would need unsafe_get"
        | Some json -> 
          let ty, x = Js_json.reify_type json in
          map ty x 
      in
      t.index <- t.index + 1;
      Some (key, value)
end 


module Encoder = struct

  type v

  let make_v (x:'a) : v = Obj.magic x 
  
  type t = v Js_dict.t 

  let empty () : t = Js_dict.empty () 

  let set_null t key = 
    Js_dict.set t key (make_v Js_null.empty) 

  let set_string t key (v:string)  = 
    Js_dict.set t key (make_v v)
  
  let set_float t key (v:float)  = 
    Js_dict.set t key (make_v v)
  
  let set_int t key (v:int)  = 
    Js_dict.set t key (make_v v)
  
  let set_bool t key (v:bool)  = 
    Js_dict.set t key (make_v (Js_boolean.to_js_boolean v))

  let set_object t key (v:t) = 
    Js_dict.set t key (make_v v)

  let set_string_list t key (v:string list)  = 
    Js_dict.set t key (make_v (Array.of_list v))
  
  let set_float_list t key (v:float list)  = 
    Js_dict.set t key (make_v (Array.of_list v))
  
  let set_int_list t key (v:int list)  = 
    Js_dict.set t key (make_v (Array.of_list v))
  
  let set_bool_list t key (v:bool list)  = 
    Array.of_list v 
    |> Js_array.map Js_boolean.to_js_boolean 
    |> make_v 
    |> Js_dict.set t key 

  let set_object_list t key (v:t list) = 
    Js_dict.set t key (make_v v) 

  external to_string : 'a -> string = "JSON.stringify" [@@bs.val]
  
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

let () = 
  let encoder = Encoder.empty () in 
  Encoder.set_null encoder "v_null"; 
  Encoder.set_string encoder "v_string" "the string"; 
  Encoder.set_float encoder "v_float" 1.0000123; 
  Encoder.set_int encoder "v_int" 123; 
  Encoder.set_bool encoder "v_bool_true" true; 
  Encoder.set_bool encoder "v_bool_false" false; 
  Encoder.set_string_list encoder "v_string_list" ["a"; "ab"; "abc"];
  Encoder.set_float_list encoder "v_float_list" [1.2;2.3;3.40001];
  Encoder.set_int_list encoder "v_int_list" [1;2;3;123];
  Encoder.set_bool_list encoder "v_bool_list" [true;false;true];

  encoder |> Encoder.to_string |> print_endline
