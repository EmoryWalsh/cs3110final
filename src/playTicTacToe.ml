open TicTacToeBoard

let instructions =
  "Welcome to Tic Tac Toe.\n\
  \  \n\
   The object of Tic Tac Toe is to get three in a row. You play on a \
   three by three game board. The first player is known as X and the \
   second is O. Players alternate placing Xs and Os on the game board \
   until either oppent has three in a row or all nine squares are \
   filled. X always goes first, and in the event that no one has three \
   in a row, the stalemate is called a cat game. Player X will go \
   first, and player O will follow.\n\n"

(** [check_str s] checks to see whether [s] is the string of an integer. *)
let check_str s =
  try
    int_of_string s |> ignore;
    true
  with
  | Failure _ -> false

let rec do_move state (player : players) =
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("The board currently looks like: \n" ^ board_state state
   ^ "\nPlayer "
    ^ Char.escaped (player_match player)
    ^ ", please choose the square where you would like to place your \
       piece.");
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Goodbye. Thanks for playing my game.\n"
  | move ->
      if check_str move then
        let move_int = int_of_string move in
        let new_state =
          place_piece (player_match player) move_int state
        in
        match new_state with
        | Illegal ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "Not a legal move.\n";
            do_move state player
        | Legal t -> (
            let winner = is_winner t in
            match winner with
            | X ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  (board_state t
                 ^ "\n\
                    Congratulations player X, you have won the game. \n"
                  )
            | O ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  (board_state t
                 ^ "\n\
                    Congratulations player O, you have won the game. \n"
                  )
            | Tie ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  (board_state t
                 ^ "This is a cat game. There is no winner.")
            | Nil -> do_move t (next_player player))
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Not a legal move because your input is not an integer.\n"

let play =
  ANSITerminal.print_string [ ANSITerminal.red ] instructions;
  do_move init_board X