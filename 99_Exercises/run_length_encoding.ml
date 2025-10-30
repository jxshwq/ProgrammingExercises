(* # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
 *)

let encode list =
    let rec aux current acc = function
      | [] -> []
      | [x] -> ((List.length current)+1, x) :: acc
      | a :: (b :: _ as t) ->
         if a = b then aux (a :: current) acc t
         else aux [] (((List.length current)+1, a) :: acc) t  in
    List.rev (aux [] [] list);;