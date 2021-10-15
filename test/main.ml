open OUnit2
open Game
open TicTacToeBoard

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

let board_state_test name exp state =
  name >:: fun _ -> assert_equal exp (board_state state)

let board_state_tests =
  [
    board_state_test "Empty Board" empty_ttt_rep init_board;
    board_state_test "Test Board" test_ttt_rep test_board;
  ]

let ttt_tests = board_state_tests

(* ************ END OF TIC TAC TOE TESTS ************ *)

let hangman_tests = []

let connect_four_tests = []

let suite =
  "test suite for Final Project"
  >::: List.flatten [ ttt_tests; hangman_tests; connect_four_tests ]

let _ = run_test_tt_main suite
