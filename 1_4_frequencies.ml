(* Let's write a function (or a pool of functions) that given a quite large text (over than 2000 words) 
counts how frequent each word occurs in the text.

The text is read from a file (look at the pervasive module in the manual) and it is a real text 
with punctuation (i.e., commas, semicolons, ...) that should be counted.

Note that words with different case should be considered the same. *)

let to_lower c = String.lowercase_ascii c;;

let is_letter_or_space c = 
  let char = Char.lowercase_ascii c in
  (char >= 'a' && char <= 'z')|| c = ' ';;

let remove_punctuation s = 
  s
  |> String.to_seq
  |> Seq.filter is_letter_or_space
  |> String.of_seq;;

let split_words = String.split_on_char ' ';;

let process_text s = 
  s
  |> to_lower
  |> remove_punctuation
  |> split_words
  |> List.filter (fun word -> word <> "");;

let rec update_count word counts = 
  match counts with
  | [] -> [(word, 1)]
  | (w, n)::rest ->
    if w = word then 
      (w, n+1)::rest
    else 
      (w, n):: (update_count word rest);;

let count_words words = 
  List.fold_left
    (fun counts word -> update_count word counts)
    []
    words;;

let read_file filename = 
  In_channel.with_open_text filename In_channel.input_all;;

let word_frequency filename =
  filename
  |> read_file         
  |> process_text      
  |> count_words;;