open TicTacToeBoard

let instructions =
  "Welcome to Tic Tac Toe.\n\
  \  \n\
  \  The object of Tic Tac Toe is to get three in a row. You play on a \
   three by three game board. The first player is known as X and the \
   second is O. Players alternate placing Xs and Os on the game board \
   until either oppent has three in a row or all nine squares are \
   filled. X always goes first, and in the event that no one has three \
   in a row, the stalemate is called a cat game."

(* val do_move : int -> TicTacToeBoard.t -> TicTacToeBoard.players ->
   unit *)
let rec do_move state (player : players) =
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("The board currently looks like: \n" ^ board_state state
   ^ "\nPlayer " ^ player_match player
   ^ ", please choose the square where you would like to place your \
      piece.");
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Goodbye. Thanks for playing my game.\n"
  | move -> (
      let new_state = place_piece in
      match new_state with
      | Illegal ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "Not a legal move.\n";
          do_move state player
      | Legal t -> place_piece)

let play =
  ANSITerminal.print_string [ ANSITerminal.red ] instructions;
  do_move init_board X
(* raise (Failure "Unimplemented: PlayTicTacToe.play") *)
