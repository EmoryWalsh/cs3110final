type t = {
  guessedList : char list;
  board : char list;
  word : char list;
}

let test_t =
  {
    guessedList = [];
    board = [ '_'; '_'; '_'; '_' ];
    word = [ 'W'; 'H'; 'A'; 'T' ];
  }

let rec init_board word =
  match word with
  | [] -> []
  | h :: t -> '_' :: init_board t

let initialize_t w =
  { guessedList = []; board = init_board w; word = w }

let init_t =
  {
    guessedList = [];
    board = [ '_'; '_'; '_'; '_' ];
    word = [ 'W'; 'H'; 'A'; 'T' ];
  }

let zero_guess = "  _____\n  |   |\n  |\n  |\n  |\n  |   \n----- \n"

let one_guess = "  _____\n  |   |\n  |   O\n  |\n  | \n  |   \n----- \n"

let two_guess =
  "  _____\n  |   |\n  |   O\n  |   |\n  | \n  |   \n----- \n"

let three_guess =
  "  _____\n  |   |\n  |   O\n  |  -|\n  | \n  |   \n----- \n"

let four_guess =
  "  _____\n  |   |\n  |   O\n  |  -|-\n  |  \n  |   \n----- \n"

let five_guess =
  "  _____\n  |   |\n  |   O\n  |  -|-\n  |  /\n  |   \n----- \n"

let six_guess =
  "  _____\n  |   |\n  |   O\n  |  -|-\n  |  /|\n  |   \n----- \n"

let seven_guess =
  "  _____\n  |   |\n  |   O\n  |  -|-\n  | _/|\n  |   \n----- \n"

let eight_guess =
  "  _____\n  |   |\n  |   O\n  |  -|-\n  | _/|_\n  |   \n----- \n"

let wordlist = [ "hi"; "hey"; "what"; "conspicuous" ]

exception MalformedBoard

exception InvalidIndex of int

type result =
  | Legal of t
  | Illegal of string

let rec num_wrong_guesses state guessedLst =
  match guessedLst with
  | [] -> 0
  | h :: t ->
      if List.mem h state.word then num_wrong_guesses state t
      else 1 + num_wrong_guesses state t

let rec wrong_list state guessedLst =
  match guessedLst with
  | [] -> []
  | h :: t ->
      if List.mem h state.word then wrong_list state t
      else h :: wrong_list state t

let rec update_board letter state =
  let () = print_endline "\n\n\n\n\n\n\n\n" in
  List.map2
    (fun x y -> if x = letter then letter else y)
    state.word state.board

let rec right_list state guessedLst =
  match guessedLst with
  | [] -> []
  | h :: t ->
      if List.mem h state.word then h :: wrong_list state t
      else wrong_list state t

let word_length t : int = List.length t.word

let rec build_board state board =
  match board with
  | [] -> ""
  | h :: t -> "  " ^ Char.escaped h ^ build_board state t

let rec repr_guessed_list t guessedList =
  match guessedList with
  | [] -> ""
  | [ x ] -> Char.escaped x ^ ""
  | h :: tail -> Char.escaped h ^ ", " ^ repr_guessed_list t tail

let repr_board_state t =
  let hangman_repr =
    match num_wrong_guesses t t.guessedList with
    | 0 -> zero_guess
    | 1 -> one_guess
    | 2 -> two_guess
    | 3 -> three_guess
    | 4 -> four_guess
    | 5 -> five_guess
    | 6 -> six_guess
    | 7 -> seven_guess
    | 8 -> eight_guess
    | _ -> failwith "Guess index out of bounds"
  in
  let word_repr = build_board t t.board in
  let message =
    "You have "
    ^ string_of_int (8 - num_wrong_guesses t t.guessedList)
    ^ " guesses left."
  in
  let guessed = repr_guessed_list t t.guessedList in
  hangman_repr ^ "\n\n" ^ word_repr ^ "\n\n" ^ message ^ "\n\n"
  ^ "So far, you have guessed: " ^ guessed ^ "\n\n"

let valid_letter letter =
  match letter with
  | 'A' .. 'Z' -> true
  | _ -> false

let guess_letter letter t =
  if List.mem letter t.guessedList then
    Illegal "\n\n\nYou've already guessed this letter. Please retry.\n"
  else if not (valid_letter letter) then
    Illegal "\n\n\nYou must enter a character A..Z. \n"
  else if valid_letter letter then
    let new_guessed = letter :: t.guessedList in
    let new_board = update_board letter t in
    Legal
      { guessedList = new_guessed; board = new_board; word = t.word }
  else Illegal "not valid"

let has_won t = if List.mem '_' t.board then false else true

let has_lost t =
  if num_wrong_guesses t t.guessedList = 8 then true else false
