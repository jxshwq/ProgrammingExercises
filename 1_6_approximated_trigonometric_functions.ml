(* sin(x) can be approximate by the Taylor's series:

Similarly you can approximate all the trigonometric and transcendent 
functions (look at:

http://en.wikipedia.org/wiki/Taylor_series).

Let's write a module to implement sin x n by using the Taylor's 
series (where n is the level of approximation, i.e., 1 only one item, 
2 two items, 3 three items and so on). Do the same with cosine,
 tangent, logarithm and so on.

Let's compare your functions with those implemented in the pervasive 
module at the growing of the approximation level. *)

let rec factorial n =
  match n with 
  | 0 -> 1
  | _ -> n * factorial (n-1);;

let rec power x n = 
  match n with 
  | 0 -> 1.0
  | _ -> x *. (power x (n-1))

let sin_term x i =
  let n = 2 * i +1 in 
  let sign = if i mod 2 = 0 then 1. else -1. in
  let numerator = power x n in 
  let denominator = factorial n in 
  sign *. numerator /. (float_of_int denominator)

let sin_taylor x n =
  let rec sum_terms i = 
    if i >= n then 0.0
    else sin_term x i +. sum_terms (i + 1)
  in sum_terms 0