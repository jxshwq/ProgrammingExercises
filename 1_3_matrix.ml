(* Write the matrix datatype with the following operations:

    A function zeroes to construct a matrix of size n×m filled with zeros.
    A function identity to construct the identity matrix (the one with all 0s but the 1s on the diagonal) of given size.
    A function init to construct a square matrix of a given size n filled with the first n×n integers.
 *)

type matrix = int list list

let zeroes m =
  let rec zeroes lista  =   
    if m > List.length lista then zeroes ( 0::lista ) 
    else lista 
  in zeroes [];;

let createZeroesMatrix m n =
  let rec createZeroesMatrix matrice =
    if n > List.length matrice then createZeroesMatrix ( zeroes m ::matrice ) 
    else matrice
  in createZeroesMatrix [];;


let ones m i= 
  let rec ones lista j =
    if m > List.length lista then
      if i = j then ones ( 1::lista) (j+1)
      else ones (0::lista) (j+1)
    else List.rev lista
  in ones [] 0

let identity m = 
let rec identity matrice i = 
  if i < m then identity ( ones m i:: matrice) (i + 1)
  else List.rev matrice
in identity [] 0;;

let crescendo m start =
  let rec build lista current = 
    if m > List.length lista then
      build (current::lista) (current +1)
    else List.rev lista
  in build [] start

let init m =
  let rec init matrice i =
    if i < m then init (crescendo m (m*i)::matrice) (i+1)
    else List.rev matrice
  in init [] 0