(* Eliminate consecutive duplicates of list 
elements. *)
 let compress l =
  let rec aux l result = 
  match l with 
  | [] -> List.rev result
  | [x] -> List.rev (x::result)
  | h::h2::t -> if h <> h2 then aux (h2::t) (h::result)
                else aux (h2::t) result
  in aux l [] 

(* 
un altra soluzione è la seguente:  *)
(* let rec compress = function
  | a::(b::_ as t) -> if a = b then compress t else a::compress t
  | x -> x *)


