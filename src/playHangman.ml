open HangmanBoard

let instructions =
  "Welcome to Hangman.\n\
  \  \n\
   One player thinks of a word or phrase; the other tries to guess \
   what it is one letter at a time. The number of dashes shown will be \
   equivalent to the number of letters in the word. If the guessing \
   player suggests a letter that occurs in the word, the dash is \
   filled in the blanks with that letter in the right places. If the \
   word does not contain the suggested letter, the computer draws one \
   element of a hangman’s body. As the game progresses, a segment of \
   the body and of a victim is added for every suggested letter not in \
   the word. If the player guesses then they win. Otherwise the player \
   who chose the word wins.\n\n"

let check_char c =
  try
    String.get c 0 |> Char.escaped |> ignore;
    String.get c 0 |> Char.uppercase_ascii |> ignore;
    true
  with
  | Failure _ -> false

let get_error r =
  match r with
  | Illegal str -> str
  | _ -> ""

let rec do_move state =
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("The board currently looks like: \n" ^ repr_board_state state
   ^ "Please now guess a letter: ");
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Goodbye. Thanks for playing my game.\n"
  | guess_str ->
      if check_char guess_str then
        let guess = Char.uppercase_ascii (String.get guess_str 0) in
        let new_state = guess_letter guess state in
        match new_state with
        | Illegal str ->
            ANSITerminal.print_string [ ANSITerminal.red ] (str ^ "\n");
            do_move state
        | Legal t ->
            let win_state = has_won t in
            let lose_state = has_lost t in
            if win_state then
              ANSITerminal.print_string [ ANSITerminal.red ]
                (repr_board_state t
               ^ "\nCongratulations player, you have won the game. \n")
            else if lose_state then
              ANSITerminal.print_string [ ANSITerminal.red ]
                (repr_board_state t
               ^ "\nPlayer, you have lost the game. \n")
            else do_move t
      else (
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Not a legal move because your input is not an integer.\n";
        do_move state)

let rec convert n w =
  match n with
  | -1 -> []
  | _ -> convert (n - 1) w @ [ String.get w n ]

let rec word_to_list w =
  let num = String.length w in
  let stringlist =
    List.map (fun x -> Char.escaped x) (convert (num - 1) w)
  in
  convert (num - 1) (String.uppercase_ascii w)

let valid_word w =
  let charlist = word_to_list w in
  let strlist =
    List.for_all
      (fun x ->
        match x with
        | 'a' .. 'z' -> true
        | 'A' .. 'Z' -> true
        | _ -> false)
      charlist
  in
  strlist

let rec get_word () =
  let word_try =
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Player 1, please input one word with only letters a..z or A..Z: "
  in
  let word = read_line () in
  let hi =
    print_string
      "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
  in
  match valid_word word with
  | true -> word_to_list word
  | false ->
      print_string "Sorry, that was an invalid word.";
      get_word ()

let play () =
  ANSITerminal.print_string [ ANSITerminal.red ] instructions;
  let word = get_word () in
  do_move (initialize_t word)