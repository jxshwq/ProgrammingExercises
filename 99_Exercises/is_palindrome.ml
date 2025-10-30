
let rev lst = 
  let rec aux lst result = 
    match lst with 
    | [] -> result 
    | h::t -> aux t (h::result)
  in aux lst []


let is_palindrome s = 
  s = rev s