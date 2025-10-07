
(* Put into a list, called alkaline_earth_metals, the atomic numbers of the six alkaline earth metals: beryllium (4), magnesium (12), calcium (20), strontium (38), barium (56), and radium (88). Then

    Write a function that returns the highest atomic number in alkaline_earth_metals.
    Write a function that sorts alkaline_earth_metals in ascending order (from the lightest to the heaviest).

Put into a second list, called noble_gases, the noble gases: helium (2), neon (10), argon (18), krypton (36), xenon (54), and radon (86). Then

    Write a function (or a group of functions) that merges the two lists and print the result as couples (name, atomic number) sorted in ascending order on the element names.
 *)
let alkaline_earth_metals = [4;12;20;38;56;88]

let noble_gases = [("helium", 2); ("neon", 10); ("argon", 18);
                   ("krypton", 36); ("xenon", 54); ("radon", 86)]

let alkaline_earth_metals_named = [
  ("beryllium", 4);
  ("magnesium", 12);
  ("calcium", 20);
  ("strontium", 38);
  ("barium", 56);
  ("radium", 88)
]

let mergedList = alkaline_earth_metals_named @ noble_gases


let rec max_in_list list = 
  match list with
    | [] -> failwith "Empty list has no maximum"
  | [x] -> x
  | hd :: tl -> 
    let m = max_in_list tl in 
    if snd hd > snd m then hd else m


let rec remove x list = 
  match list with
  | [] -> []
  | hd :: tl -> if hd = x then tl else hd::remove x tl

let rec sort_list list =
  match list with
  | [] -> []
  | _ ->
      let num = max_in_list list in
      let rest_sorted = sort_list (remove num list) in
      rest_sorted @ [num]

let () =
  let sorted = sort_list mergedList in
 List.iter (fun (name, num) ->
  print_string name;
  print_string " ";
  print_int num;
  print_newline ()
) sorted

