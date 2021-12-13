(** Representation of a Tic Tac Toe Board.

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
  | X
  | O  (** Represents the different players of the game. *)

type winner =
  | X
  | O
  | Tie
  | Nil  (** Represents the winner of the game. *)

val init_board : t
(** [init_state] is the initial state of a game of tic tac toe. It has
    no tokens placed on the board and each square has a index 1-9. *)

val next_player : players -> players
(** [next_player p] is the next player to make a move. *)

val test_board : t
(** [test_board] is an example of a board with some spots filled as an
    example. *)

val player_match : players -> char
(** [player_match p] is the string representation of a player. *)

type result =
  | Legal of t
  | Illegal
      (** The type representing the result of an attempted movement. *)

type plays = {
  x : int list;
  o : int list;
  nil : int list;
}
(** Data type representing the plays made by each player*)

val plays : t -> plays
(** [plays state] gives a representation of all of the different moves
    that have occured thus far.*)

val board_state : t -> string
(** [board_state s] creates a string representation of a tic tac toe
    board [s]. *)

val place_piece : char -> int -> t -> result
(** [place_piece player i state] is the new state representation when
    [player] adds a piece to board [state] at index [i]. Raises
    [InvalidIndex i] when [i] is out of bounds or already played. *)

val is_winner : t -> winner
(** [is_winner s] checks whether either player has won the game in state
    [s] and returns the player who has won or nil if neither have won. *)
