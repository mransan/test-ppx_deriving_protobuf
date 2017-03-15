open Example01_pb 

let () = 
  let p = {
    name = "Maxime Ransan";
    id = 12344l;
    email = Some "maxime.ransan@gmail.com";
    phone = ["9179298071"; "6462692829"]; 
  } in 
  
  p
  |> Format.asprintf "%a" pp_person 
  |> print_endline 
