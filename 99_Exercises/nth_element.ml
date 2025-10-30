(* Find the N'th element of a list. *)

let rec at k lst = 
  match lst with
  | [] -> None
  | h::t -> if k = 0 then Some h else at (k-1) t