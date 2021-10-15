(** Representation of a Tic Tac Toe Board.

    This module represents the state of an board as it is being played,
    including the current player, the pieces that have been placed, and
    functions that cause the state to change. *)

type t
(** The abstract type of values representing the game state. *)

val init_board : t
(** [init_state] is the initial state of a game of tic tac toe. It has
    no tokens placed on the board and each square has a index 1-9. *)

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of t
  | Illegal