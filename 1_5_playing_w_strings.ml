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

(* let char_in_string c s = 
    let rec char_in_string_helper = 
        match c with
        | [] -> true
        | a::str *)