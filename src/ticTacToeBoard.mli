(** Representation of a Tic Tac Toe Board.

    This module represents the state of an board as it is being played,
    including the current player, the pieces that have been placed, and
    functions that cause the state to change. *)

type t
(** The abstract type of values representing the game state. *)

exception MalformedBoard

exception InvalidIndex of int

val init_board : t
(** [init_state] is the initial state of a game of tic tac toe. It has
    no tokens placed on the board and each square has a index 1-9. *)

val test_board : t
(** [test_board] is an example of a board with some spots filled as an
    example. *)

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of t
  | Illegal

val board_state : t -> string
(** [board_state s] creates a string representation of a tic tac toe
    board [s]. *)

val place_piece : char -> int -> t -> t
(** [place_piece player i state] is the new state representation when
    [player] adds a piece to board [state] at index [i]. Raises
    [InvalidIndex i] when [i] is out of bounds or already played. *)
