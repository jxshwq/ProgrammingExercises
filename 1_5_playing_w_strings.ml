(* Define the following functions/operators on strings:

    is_palindrome: string → bool that checks if the string is palindrome, a string is palindrome
         when the represented sentence can be read the same way in either directions in spite of 
        spaces, punctual and letter cases, e.g., detartrated, "Do geese see God?", "Rise to vote, 
        sir.", ...

    operator (-): string → string → string that subtracts the letters in a string from 
    the letters in another string, e.g., "Walter Cazzola"-"abcwxyz" will give "Wlter Col"
     note that the operator - is case sensitive


    anagram : string → string list → boolean that given a dictionary of strings, checks if the input string is an anagram of one or more of the strings in the dictionary
 *)

let rec is_palindrome word = 
    let word1 = String.trim word in 
    if String.length word1 = 1 || String.length word1 = 0 then true
    else if word1.[0] <> word1.[String.length word1-1] then false
    else 
        let newWord = String.sub word1 1 (String.length word1 - 2)
    in  is_palindrome newWord

let char_in_string = String.contains

let (-) s1 s2 = 
    s1
    |> String.to_seq
    |> Seq.filter (fun c -> not (String.contains s2 c))
    |> String.of_seq

let rec update_char_count c counts = 
    match counts with 
    | [] -> [(c, 1)]
    | (ch, n):: rest -> if c = ch then (ch, n+1):: rest
                        else (ch, n) :: (update_char_count c rest) 

let rec checkPresenza char lista =
    match lista with 
    | [] -> false
    | hd::tl -> if char = hd then true else checkPresenza char tl

let count_char s = 
    s
    |> String.to_seq
    |> List.of_seq
    |> List.fold_left (fun counts c -> update_char_count c counts) [] 

let compare_by_char (c1, _) (c2, _) = 
  Char.compare c1 c2;;

let sort_counts lst = 
  List.sort compare_by_char lst;;

let are_anagrams s1 s2 = 
    let counts1 = count_char s1 |> sort_counts in 
    let counts2 = count_char s2 |> sort_counts in
    counts1 = counts2 