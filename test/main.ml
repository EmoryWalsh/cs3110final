open OUnit2
open Game
open TicTacToeBoard
open HangmanBoard

(* ************ TIC TAC TOE TESTS ************ *)

(* Tic Tac Toe Testing States *)
let empty_ttt_rep =
  " 1 | 2 | 3\n\
   ___|___|___\n\
  \ 4 | 5 | 6\n\
   ___|___|___\n\
  \ 7 | 8 | 9\n\
  \   |   |   \n"

let test_ttt_rep =
  " O | O | 3\n\
   ___|___|___\n\
  \ 4 | X | 6\n\
   ___|___|___\n\
  \ 7 | X | 9\n\
  \   |   |   \n"

(* Tic Tac Toe Testing Functions and Tests *)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let board_state_test name exp state =
  name >:: fun _ -> assert_equal exp (board_state state)

let board_state_tests =
  [
    board_state_test "Empty Board" empty_ttt_rep init_board;
    board_state_test "Test Board" test_ttt_rep test_board;
  ]

let next_player_test name exp player =
  name >:: fun _ -> assert_equal exp (next_player player)

let next_player_tests =
  [
    next_player_test "Input X, should get O" X O;
    next_player_test "Input O, should get X" O X;
  ]

let player_match_test name exp player =
  name >:: fun _ -> assert_equal exp (player_match player)

let player_match_tests =
  [
    player_match_test "Player X" 'X' X;
    player_match_test "Player O" 'O' O;
  ]

let plays_test name exp state =
  let p = plays state in
  name >:: fun _ ->
  assert_equal
    (cmp_set_like_lists p.x exp.x
    && cmp_set_like_lists p.o exp.o
    && cmp_set_like_lists p.nil exp.nil)
    true

let plays_tests =
  [
    plays_test "Empty Board"
      { o = []; x = []; nil = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] }
      init_board;
    plays_test "Test Board"
      { o = [ 1; 2 ]; x = [ 5; 8 ]; nil = [ 3; 4; 6; 7; 9 ] }
      test_board;
  ]

let place_piece_test name player i exp state =
  let placed = place_piece player i state in
  name >:: fun _ ->
  match placed with
  | Illegal -> assert_equal exp { x = []; o = []; nil = [] }
  | Legal t -> assert_equal exp (plays t)

let t1 = { x = [ 5 ]; o = []; nil = [ 1; 2; 3; 4; 6; 7; 8; 9 ] }

let t2 = { x = [ 5; 8 ]; o = [ 1; 2; 6 ]; nil = [ 3; 4; 7; 9 ] }

let fail_t = { x = []; o = []; nil = [] }

let place_piece_tests =
  [
    place_piece_test "Place piece on empty board" 'X' 5 t1 init_board;
    place_piece_test "Place piece in a full spot" 'X' 2 fail_t
      test_board;
    place_piece_test "Place piece on partially full board" 'O' 6 t2
      test_board;
    place_piece_test "Place piece on nonexistent square" 'O' 0 fail_t
      test_board;
    place_piece_test "Place piece on nonexistent square" 'O' 100 fail_t
      test_board;
  ]

let ttt_tests =
  board_state_tests @ next_player_tests @ player_match_tests
  @ plays_tests @ place_piece_tests

(* ************ END OF TIC TAC TOE TESTS ************ *)

let empty_hangman_repr =
  "  _____              \n\
   |   |\n\
   |\n\
   |\n\
   |\n\
   |   \n\
   ----- \n\n\n\
   _  _  _  _\n\n\
   You have 8 guesses left.\n\n"

let correct_guess_hangman_repr =
  "  _____              \n\
   |   |\n\
   |\n\
   |\n\
   |\n\
   |   \n\
   ----- \n\n\n\
   _  _  A  _\n\n\
   You have 8 guesses left.\n\n\
   So far, you have guessed: A\n\n"

let wrong_guess_hangman_repr =
  "  _____\n\
   |   |\n\
   |   O\n\
   |\n\
   | \n\
   |   \n\
   ----- \n\n\n\
   _  _  A  _\n\n\
   You have 7 guesses left.\n\n\
   So far, you have guessed: B, A\n\n"

let get_t state =
  match state with
  | Legal t -> t
  | _ -> failwith "not legal"

let repr_board_state_test name exp state =
  name >:: fun _ -> assert_equal exp (repr_board_state (get_t state))

let repr_board_state_tests = []

let hangman_tests = repr_board_state_tests

let connect_four_tests = []

let suite =
  "test suite for Final Project"
  >::: List.flatten [ ttt_tests; hangman_tests; connect_four_tests ]

let _ = run_test_tt_main suite
