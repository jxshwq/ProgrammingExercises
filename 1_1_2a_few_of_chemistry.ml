let alkaline_earth_metals = ("beryllium", 4)::("magnesium", 12)::("calcium", 20)::("Strontium", 38)::("barium", 56)::("radium", 88)::[];;

let noble_gases = [("helium", 2); ("radeon", 10); ("argon", 10); ("krypton", 36); ("xenon", 54); ("radon", 86)];;

let (>:) a b = 
  (snd a) - (snd b)

let max a b = if (a >: b >= 0) then a else b;;

let heaviest lst = 
  List.fold_left max (List.hd lst) (List.tl lst) ;;

let sort_ascending lst = List.sort (>:) lst ;;

let merge_elements metals gases = 
  let sorted_metals = sort_ascending metals and
    sorted_gases = sort_ascending gases in 
    List.merge (>:) sorted_metals sorted_gases;;

merge_elements alkaline_earth_metals noble_gases ;;