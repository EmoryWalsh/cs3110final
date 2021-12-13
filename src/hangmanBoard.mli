(** Representation of a Hangman Board.

    This module represents the state of an board as it is being played,
    including the guessed letters, the letters that have been placed,
    and functions that cause the state to change. *)

type t
(** The abstract type of values representing the game state. *)

val init_t1 : t
(** An initializing of an example game state for testing purposes. *)

val init_t2 : t
(** An initializing of an example game state for testing purposes. *)

val init_t3 : t
(** An initializing of an example game state for testing purposes. *)

type result =
  | Legal of t
  | Illegal of string
(** The type representing the result of an attempted letter guess. *)

val repr_board_state : t -> string
(** [board_state s] creates a string representation of a hangman
    board [s]. *)

val initialize_t : char list -> t
(** [initialize_t word_list] is the initial state of a game of hangman. It has
    an empty guessed_list, a board containing all ['_'] characters the same
    length of input word_list, and word of input word_list. *)

val guess_letter : char -> t -> result
(** [guess_letter letter state] is the new state representation when
    [player 2] guesses a letter of the unknown word. Requests another
    letter if the guessed character is invalid or has already been guessed*)

val has_won : t -> bool
(** [has_won s] checks whether either player has won the game in state
    [s] and returns the player who has won or nil if neither have won. *)

val has_lost : t -> bool
(** [has_lost s] checks whether either player has won the game in state
    [s] and returns whether the player that guessed won or the player
    that determined the unknown word won. *)
