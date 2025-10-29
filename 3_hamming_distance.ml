let string_to_list s =
  s
  |> String.to_seq
  |> List.of_seq
  
let compare_chars l1 l2 =
  let rec aux counter changes list1 list2 =

    match list1, list2 with 
    | [], []                           -> counter, changes
    | ([], _) | (_, [])                -> invalid_arg "strings must have the same length"
    | (first1::rest1), (first2::rest2) -> 
      if first1 = first2 then aux counter changes rest1 rest2 
      else aux (counter+1) ((first1, first2)::changes) rest1 rest2
    in 
    let count, result =  (aux 0 [] l1 l2) in
    (count, List.rev result)

let rec print_changes = function
  | [] -> ()
  | [(c1, c2)] -> 
      print_string (String.make 1 c1 ^ " -> " ^ String.make 1 c2)
  | (c1, c2) :: rest -> 
      print_string (String.make 1 c1 ^ " -> " ^ String.make 1 c2 ^ ", ");
      print_changes rest

let print_result counter changes = 
  print_endline ("distance: " ^ string_of_int counter);
  print_string "Changes: [";
  print_changes changes;
  print_string "]";;

let hamming s1 s2 =
  if String.length s1 <> String.length s2 then 
    invalid_arg "string must have the same length"
  else
    let list1 = string_to_list s1 in
    let list2 = string_to_list s2 in
    compare_chars list1 list2 
    (* se si vuole si può anche stampare *)
    (* print_result counter changes; *)
