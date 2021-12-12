open ConnectFourBoard

let instructions =
  "Welcome to Connect4! \n\
  \ \n\
   The object of Connect4 is to get four in a row. You play on a 6 by \
   7 game board. The first player is known as Blue (for its piece \
   being the color blue) and the second player is Red (for its piece \
   being the color red). Players alternate playing Bs and Rs on the \
   game board until either opponent has four in a row or all 42 \
   squares are filled. B always goes first, and in the event that no \
   one has four in a row, the stalemate is called a tie. Player B will \
   go first, and player R will follow.\n\n"

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
    ^ String.escaped (player_match_string player)
    ^ ", please choose the column where you would like to drop your \
       piece.");
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Goodbye. Thanks for playing my game!\n"
  | move ->
      if check_str move then
        let move_int = int_of_string move in
        let new_state =
          place_piece (player_match_char player) move_int state
        in
        match new_state with
        | Illegal ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "Not a legal move.\n";
            do_move state player
        | Legal t -> (
            let winner = is_winner t in
            match winner with
            | B ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  (board_state t
                 ^ "\n\
                    Congratulations Player Blue, you have won the \
                    game! :) \n")
            | R ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  (board_state t
                 ^ "\n\
                    Congratulations Player Red, you have won the game! \
                    :) \n")
            | Tie ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  (board_state t
                 ^ "This is a tie game. There is no winner.")
            | Nil -> do_move t (next_player player))
      else (
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Not a legal move because your input is not an integer.\n";
        do_move state player)

(** [play] starts a game of Connect4 *)
let play () =
  ANSITerminal.print_string [ ANSITerminal.red ] instructions;
  do_move init_board B