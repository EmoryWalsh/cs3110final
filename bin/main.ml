open Game

let rec prompt_play () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Please enter the name of the game you want to play. You can \
     choose from tic tac toe, hangman, and connect 4! If you'd like to \
     leave, type quit.\n\
     >";
  match read_line () with
  | exception End_of_file -> ()
  | game ->
      if String.lowercase_ascii game = "tic tac toe" then (
        PlayTicTacToe.play ();
        prompt_play ())
      else if String.lowercase_ascii game = "hangman" then (
        PlayHangman.play ();
        prompt_play ())
      else if String.lowercase_ascii game = "connect 4" then (
        PlayConnectFour.play ();
        prompt_play ())
      else if String.lowercase_ascii game <> "quit" then (
        ANSITerminal.print_string [ ANSITerminal.red ]
          (game
         ^ " is not a valid game name. Your choices are tic tac toe, \
            hangman, or connect 4.\n");
        prompt_play ())
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Goodbye. Thanks for playing our game.\n"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to our 3110 Final Project.\n";
  prompt_play ()

(* Execute the game engine. *)
let () = main ()
