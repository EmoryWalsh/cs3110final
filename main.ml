let play_game = raise (Failure "Unimplemented: Main.play_game")

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to our 3110 Final Project.\n";
  print_endline "Please enter the name of the game you want to play.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | _ ->
      print_endline "Failure unimplemented";
      play_game

(* Execute the game engine. *)
let () = main ()