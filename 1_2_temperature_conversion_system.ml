(* Beyond the well-known Celsius and Fahrenheit, there are other six temperature scales: Kelvin, Rankine, Delisle, Newton, Réaumur, and Rømer (Look at:

http://en.wikipedia.org/wiki/Comparison_of_temperature_scales

to read about them).

    Write a function that given a pure number returns a conversion table for it among any of the 8 scales.
    Write a function that given a temperature in a specified scale returns a list of all the corresponding temperatures in the other scales, note that the scale must be specified.

Hint. Define a proper datatype for the temperature.
 *)

type scale = 
  | Celsius
  | Fahrenheit
  | Kelvin
  | Rankine
  | Delisle
  | Newton
  | Reaumur
  | Romer

type temperature = scale * float

let all_scales = [Celsius; Fahrenheit; Kelvin; Rankine; Delisle; Newton; Reaumur; Romer]

let to_celsius (s:scale) (x:float) : float = 
  match s with 
  | Celsius -> x
  | Fahrenheit -> (x -. 32.0) *. (5.0 /. 9.0)
  | Kelvin -> x -. 273.15
  | Rankine -> (x -. 491.67) *. (5.0 /. 9.0)
  | Delisle -> 100.0 -. x *. (2.0 /. 3.0)
  | Newton -> x *. (100.0 /. 33.0)
  | Reaumur -> x *. (5.0 /. 4.0)
  | Romer -> (x -. 7.5) *. (40.0 /. 21.0)

let from_celsius (s:scale) (x:float) : float = 
  match s with
  | Celsius -> x
  | Fahrenheit -> x *. (9.0 /. 5.0) +. 32.0
  | Kelvin -> x +. 273.15
  | Rankine -> (x +. 273.15) *. (9.0 /. 5.0)
  | Delisle -> (100.0 -. x) *. (3.0 /. 2.0)
  | Newton -> x *. (33.0 /. 100.0)
  | Reaumur -> x *. (4.0 /. 5.0)
  | Romer -> x *. (21.0 /. 40.0) +. 7.5

let convert (src:scale) (dest:scale) (v:float) :float =
  let c = to_celsius src v in
  from_celsius dest c

let conversion_table_from_celsius (c:float) : (scale * float) list =
  List.map (fun s -> (s, from_celsius s c)) all_scales

let string_of_scale s = 
  match s with
  | Celsius -> "Celsius"
  | Fahrenheit -> "Fahrenheit"
  | Kelvin -> "Kelvin"
  | Rankine -> "Rankine"
  | Delisle -> "Delisle"
  | Newton -> "Newton"
  | Reaumur -> "Reaumur"
  | Romer -> "Rømer"


let convert_all (s, value) =
  let c = to_celsius s value in
  List.map (fun sc -> (from_celsius sc c, sc)) all_scales

let print_conversions (s, value) =
  List.iter
    (fun (v, sc) ->
      Printf.printf "%.2f %s\n" v (string_of_scale sc))
    (convert_all (s, value))

let () = 
  print_conversions (Celsius, 30.)