let length lst = 
  let rec aux i lst = 
    match lst with
    | [] -> i
    | h::t -> aux (i+1) t
  in aux 0 lst