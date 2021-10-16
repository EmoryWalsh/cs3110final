let dict = ["hi"; "yo"; "nonsense"; "zebra"] in 
let word = List.nth dict 0 in 
let blanks = String.length (word)

let take_char = 
  let out = print_endline "Now, please input a character: " in 
  read_line

let rec search_word accum c = 
  String.c


