(** Representation of a Connect4 Board.

    This module represents the state of an board as it is being played,
    including the current player, the pieces that have been placed, and
    functions that cause the state to change. *)

type t
(** The abstract type of values representing the game state. *)

exception MalformedBoard
(** Raised when the board is formatted incorrectly. *)

exception InvalidIndex of int
(** Raised when an invalid int is given to place a piece. *)

type players =
  | B
  | R  (** Represents the different players of the game. *)

type winner =
  | B
  | R
  | Tie
  | Nil  (** Represents the winner of the game. *)

val init_board : t
(** [init_board] is the initial state of a game of connect4. It has no
    tokens placed on the board.*)
val test_board : t
(** [test_board] is a possible played state of a game of connect4. It has some tokens placed on the board.*)
val filled_col_board : t
(** [filled_col_board] is a possible played state of a game of connect4. Column 1 is completely filled with tokens.*)

val next_player : players -> players
(** [next_player p] is the next player to make a move. *)

val player_match_string : players -> string
(** [player_match_string p] is the string representation of a player to
    display for game instructions. *)

val player_match_char : players -> char
(** [player_match_char p] is the char representation of a player to
    display on board. *)

type result =
  | Legal of t
  | Illegal
      (** The type representing the result of an attempted movement. *)

type plays = {
  b : int list;
  r : int list;
  nil : int list;
}
(** Data type representing the plays made by each player*)

val plays : t -> plays

val board_state : t -> string
(** [board_state board] creates the string representation of an entire
    connect4 board. *)

val place_piece : char -> int -> t -> result
(** [place_piece player col state] is the new state representation when
    [player] adds a piece to board [state] at column [col]. Raises
    [InvalidIndex col] when [col] is out of bounds or if the column is
    completely filled. *)

val is_winner : t -> winner
(** [is_winner state] checks whether either player has won the game in
    state [s] and returns the player who has won or nil if neither have
    won. *)
