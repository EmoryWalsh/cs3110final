(** Representation of a Hangman Board.

    This module represents the state of an board as it is being played,
    including the guessed letters, the letters that have been placed,
    and functions that cause the state to change. *)

type t
(** The abstract type of values representing the game state. *)

type result =
  | Legal of t
  | Illegal of string

val repr_board_state : t -> string
(** [board_state s] creates a string representation of a tic tac toe
    board [s]. *)

val init_t : t
(** [init_t] is the initial board state of a hangman board.*)

val test_t : t
(** [init_t] is a testing board state of a hangman board.*)

val initialize_t : char list -> t
(** [initialize_t chars] initalizes a board given a list of chars
    representing the word chosen by the player. *)

val guess_letter : char -> t -> result
(** [guess_letter player i state] is the new state representation when
    [player] adds a piece to board [state] at index [i]. Raises
    [InvalidIndex i] when [i] is out of bounds or already played. *)

val has_won : t -> bool
(** [has_won s] checks whether either player has won the game in state
    [s] and returns the player who has won or nil if neither have won. *)

val has_lost : t -> bool
(** [has_lost s] checks whether either player has won the game in state
    [s] and returns the player who has won or nil if neither have won. *)
