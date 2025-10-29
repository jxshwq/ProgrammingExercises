(* L'esercizio era "dato una lista di interi, una sequenza è data
da numeri consecutivi nella lista e si chiude con uno o più zeri. 
Restituire una lista con le somme delle sequenze" *)

(* Tipo per [1, 2, 3, 0, 4, 5, 0, 0, 6, 0] deve restituire [6, 9, 6] *)



let consecutive lst = 
  let rec aux lst currentSum result= 
  match lst with 
    | [] -> 
      if currentSum = 0 then List.rev result
      else List.rev (currentSum::result)
    | hd::tl -> 
      if hd = 0 then
        if currentSum = 0 then aux tl 0 result
        else aux tl 0 (currentSum::result)
      else aux tl (currentSum+hd) result
  in aux lst 0 []