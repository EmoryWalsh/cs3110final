(***************** TEST PLAN *****************)

(* To test the correctness as our game, we broke it down into three
   sections, one for each each that we made. Within each game, we tested
   each of the functions in the mli files to ensure that everything
   worked as planned. We also manually tested each game, and tried to
   throw in weird inputs that our code may not expect to deal with.
   Speciafically, we manually tested the winning states of each game
   along with boundary/unexpected inputs. To develop our OUnit tests, we
   mainly utilized black box testing and looked to make sure that inputs
   gave the expected outputs. We also did some randomized testing just
   to make sure that everything ran smoothly. Our test suite
   demonstrates correctness of the system because we spent the time
   making a well thought out test suite, and everything that couldn't be
   tested with OUnit, we did manually.*)

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

let is_winner_test name lst exp =
  name >:: fun _ -> assert_equal exp (is_winner lst)

let matched t =
  match t with
  | Legal t -> t
  | Illegal -> init_board

let t1 = place_piece 'O' 3 test_board

let t2 = place_piece 'X' 7 test_board

let t3 = place_piece 'X' 7 test_board |> matched |> place_piece 'X' 3

let t4 =
  place_piece 'X' 1 init_board
  |> matched |> place_piece 'X' 2 |> matched |> place_piece 'X' 3

let t5 =
  place_piece 'O' 1 init_board
  |> matched |> place_piece 'O' 2 |> matched |> place_piece 'O' 3

let t6 =
  place_piece 'O' 9 init_board
  |> matched |> place_piece 'O' 8 |> matched |> place_piece 'O' 7

let t7 =
  place_piece 'X' 9 init_board
  |> matched |> place_piece 'X' 3 |> matched |> place_piece 'X' 6

let t8 =
  place_piece 'X' 9 init_board
  |> matched |> place_piece 'X' 1 |> matched |> place_piece 'X' 5

let t9 =
  place_piece 'X' 5 init_board
  |> matched |> place_piece 'X' 3 |> matched |> place_piece 'X' 7

let t10 =
  place_piece 'X' 1 init_board
  |> matched |> place_piece 'O' 3 |> matched |> place_piece 'X' 6
  |> matched |> place_piece 'X' 6 |> matched |> place_piece 'O' 6
  |> matched |> place_piece 'O' 2 |> matched |> place_piece 'X' 5
  |> matched |> place_piece 'O' 4 |> matched |> place_piece 'X' 7
  |> matched |> place_piece 'O' 9 |> matched |> place_piece 'X' 8

let is_winner_tests =
  [
    is_winner_test "Empty board" init_board Nil;
    is_winner_test "Not a winning state" (matched t2) Nil;
    is_winner_test "Winning state" (matched t1) O;
    is_winner_test "Wnning state" (matched t3) X;
    is_winner_test "Winning state" (matched t4) X;
    is_winner_test "Winning state" (matched t5) O;
    is_winner_test "Winning state" (matched t6) O;
    is_winner_test "Winning state" (matched t7) X;
    is_winner_test "Winning state" (matched t8) X;
    is_winner_test "Winning state" (matched t9) X;
    is_winner_test "Winning state" (matched t10) Nil;
  ]

let ttt_tests =
  board_state_tests @ next_player_tests @ player_match_tests
  @ plays_tests @ place_piece_tests @ is_winner_tests

(* ************ END OF TIC TAC TOE TESTS ************ *)

(* ************ HANGMAN TESTS ************ *)
open HangmanBoard

let empty_hangman_repr =
"  _____             
  |   |
  |
  |
  |
  |   
----- 


  _  _  _  _

You have 8 guesses left.

So far, you have guessed: 

"

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

let matched t =
  match t with
  | Legal t -> t
  | Illegal i -> init_t1

let repr_board_state_test name exp state =
  name >:: fun _ -> assert_equal exp (repr_board_state state)

let repr_board_state_tests =
  [ (* repr_board_state_test "Empty board" empty_hangman_repr Legal
       init_t; *) 
  ]

let guess_letter_test name letter st exp =
  name >:: fun _ ->
  (* if exp <> guess_letter letter st then print_endline (print_res (*
     (guess_letter letter st)); *) *)
  assert_equal exp (guess_letter letter st)

let st1 = HangmanBoard.init_t1

let st2 = guess_letter 'E' st1

let st3 = guess_letter 'X' (st2 |> matched)

let st4 = guess_letter 'A' st1

let st5 = guess_letter 'T' 
  (get_t (guess_letter 'H'
  (get_t (guess_letter 'W' (get_t st4)))))

let st6 = 
(guess_letter 'P' 
(get_t (guess_letter 'X' 
(get_t (guess_letter 'F' 
(get_t (guess_letter 'E' 
(get_t (guess_letter 'D' 
(get_t (guess_letter 'Z' 
(get_t (guess_letter 'B'
(get_t (guess_letter 'C' 
(get_t st4))))))))))))))))


let guess_letter_tests =
  [
    guess_letter_test "Place 1 on init board" 'e' st1
      (Illegal "\n\n\nYou must enter a character A..Z. \n");
    guess_letter_test "Place . on init board" '.' st1
      (Illegal "\n\n\nYou must enter a character A..Z. \n");
    guess_letter_test "Place e on init board" 'E' st1 st2;
    guess_letter_test "Place e on board with e" 'E' (st2 |> matched)
      (Illegal
         "\n\n\nYou've already guessed this letter. Please retry.\n");
    guess_letter_test "Place X on board with E" 'X' (st2 |> matched) st3;
    guess_letter_test "Place tab on init board" '\t' st1
      (Illegal "\n\n\nYou must enter a character A..Z. \n");
    guess_letter_test "Place tab on init board" '\t' st1
      (Illegal "\n\n\nYou must enter a character A..Z. \n");
  ]


let has_won_test name st exp=
name >:: fun _ ->
assert_equal exp (HangmanBoard.has_won st)

let has_won_tests = [
  has_won_test "has empty board won" st1 false;
  has_won_test "has one incorrect letter won" (get_t st2) false;
  has_won_test "has one correct letter won" (get_t st4) false;
  has_won_test "has completed word won" (get_t st5) true;
  has_won_test "has ran out of guesses won" (get_t st6) false
]

let has_lost_test name st exp=
name >:: fun _ ->
assert_equal exp (HangmanBoard.has_lost st)

let has_lost_tests = [
  has_lost_test "has empty board lost" st1 false;
  has_lost_test "has one incorrect letter lost" (get_t st2) false;
  has_lost_test "has one correct letter lost" (get_t st4) false;
  has_lost_test "has completed word lost" (get_t st5) false;
  has_lost_test "has ran out of guesses lost" (get_t st6) true
]

let initialize_t_test name word exp =
name >:: fun _ ->
assert_equal exp (HangmanBoard.initialize_t word)

let init2 = HangmanBoard.init_t2

let init3 = HangmanBoard.init_t3

let initialize_t_tests = [
  initialize_t_test "initialize a word" ['W'; 'H';'A'; 'T'] st1;
  initialize_t_test "initialize a single letter" ['H'] init2;
  initialize_t_test "initialize repeating letters" ['A'; 'A'; 'A'] init3;
]


let hangman_tests = repr_board_state_tests @ guess_letter_tests 
@has_won_tests @has_lost_tests @initialize_t_tests

(* ************ END OF HANGMAN TESTS ************ *)

(* ************ CONNECT 4 TESTS ************ *)
open ConnectFourBoard

let empty_connect4_board =
  " 1 | 2 | 3 | 4 | 5 | 6 | 7\n\
  \   |   |   |   |   |   |  \n\
   ___|___|___|___|___|___|___\n\
  \   |   |   |   |   |   |  \n\
   ___|___|___|___|___|___|___\n\
  \   |   |   |   |   |   |  \n\
   ___|___|___|___|___|___|___\n\
  \   |   |   |   |   |   |  \n\
   ___|___|___|___|___|___|___\n\
  \   |   |   |   |   |   |  \n\
   ___|___|___|___|___|___|___\n\
  \   |   |   |   |   |   |  \n\
  \   |   |   |   |   |   |     \n"

let played_connect4_board =
  " 1 | 2 | 3 | 4 | 5 | 6 | 7\n\
  \   |   |   |   |   |   |  \n\
   ___|___|___|___|___|___|___\n\
  \   |   |   |   |   |   |  \n\
   ___|___|___|___|___|___|___\n\
  \   |   |   |   |   |   |  \n\
   ___|___|___|___|___|___|___\n\
  \   |   |   |   |   |   |  \n\
   ___|___|___|___|___|___|___\n\
  \ R | B | B |   |   | R |  \n\
   ___|___|___|___|___|___|___\n\
  \ B | R | R |   |   | B |  \n\
  \   |   |   |   |   |   |     \n"

let connect_board_state_test name exp state =
  name >:: fun _ -> assert_equal exp (board_state state)

let connect_board_state_tests =
  [
    connect_board_state_test "Empty Connect Board" empty_connect4_board
      init_board;
    connect_board_state_test "Played Connect Board"
      played_connect4_board test_board;
  ]

let connect_next_player_test name exp p =
  name >:: fun _ -> assert_equal exp (next_player p)

let connect_next_player_tests =
  [
    connect_next_player_test "Player after B is R" R B;
    connect_next_player_test "Player after R is B" B R;
  ]

let connect_player_match_string_test name exp p =
  name >:: fun _ -> assert_equal exp (player_match_string p)

let connect_player_match_string_tests =
  [
    connect_player_match_string_test "Player B is Blue" "Blue" B;
    connect_player_match_string_test "Player R is Red" "Red" R;
  ]

let connect_player_match_char_test name exp p =
  name >:: fun _ -> assert_equal exp (player_match_char p)

let connect_player_match_char_tests =
  [
    connect_player_match_char_test "Player B is B" 'B' B;
    connect_player_match_char_test "Player R is R" 'R' R;
  ]

let connect_cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort compare lst1 in
  let uniq2 = List.sort compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let connect_plays_test name exp state =
  let p = plays state in
  name >:: fun _ ->
  assert_equal
    (connect_cmp_set_like_lists p.b exp.b
    && connect_cmp_set_like_lists p.r exp.r
    && connect_cmp_set_like_lists p.nil exp.nil)
    true

let connect_plays_tests =
  [
    connect_plays_test "Empty Board Plays"
      {
        b = [];
        r = [];
        nil =
          [
            1;
            2;
            3;
            4;
            5;
            6;
            7;
            6;
            12;
            18;
            24;
            30;
            36;
            42;
            5;
            11;
            17;
            23;
            29;
            35;
            41;
            4;
            10;
            16;
            22;
            28;
            34;
            40;
            3;
            9;
            15;
            21;
            27;
            33;
            39;
            2;
            8;
            14;
            20;
            26;
            32;
            38;
            1;
            7;
            13;
            19;
            25;
            31;
            37;
          ];
      }
      init_board;
    connect_plays_test "Test Board Plays"
      {
        b = [ 8; 14; 1; 31 ];
        r = [ 2; 32; 7; 13 ];
        nil =
          [
            1;
            2;
            3;
            4;
            5;
            6;
            7;
            6;
            12;
            18;
            24;
            30;
            36;
            42;
            5;
            11;
            17;
            23;
            29;
            35;
            41;
            4;
            10;
            16;
            22;
            28;
            34;
            40;
            3;
            9;
            15;
            21;
            27;
            33;
            39;
            20;
            26;
            38;
            19;
            25;
            37;
          ];
      }
      test_board;
  ]

let place_piece_test name player col exp state =
  let placed = place_piece player col state in
  name >:: fun _ ->
  match placed with
  | Illegal -> assert_equal exp { b = []; r = []; nil = [] }
  | Legal t -> assert_equal exp (plays t)

let c1 =
  {
    b = [ 19 ];
    r = [];
    nil =
      [
        1;
        2;
        3;
        4;
        5;
        6;
        7;
        6;
        12;
        18;
        24;
        30;
        36;
        42;
        5;
        11;
        17;
        23;
        29;
        35;
        41;
        4;
        10;
        16;
        22;
        28;
        34;
        40;
        3;
        9;
        15;
        21;
        27;
        33;
        39;
        2;
        8;
        14;
        20;
        26;
        32;
        38;
        1;
        7;
        13;
        25;
        31;
        37;
      ];
  }

let c2 =
  {
    b = [ 9; 8; 14; 1; 31 ];
    r = [ 2; 32; 7; 13 ];
    nil =
      [
        1;
        2;
        3;
        4;
        5;
        6;
        7;
        6;
        12;
        18;
        24;
        30;
        36;
        42;
        5;
        11;
        17;
        23;
        29;
        35;
        41;
        4;
        10;
        16;
        22;
        28;
        34;
        40;
        3;
        15;
        21;
        27;
        33;
        39;
        20;
        26;
        38;
        19;
        25;
        37;
      ];
  }

let fail_c = { b = []; r = []; nil = [] }

let place_piece_tests =
  [
    place_piece_test "Place B in col 4 in empty board" 'B' 4 c1
      init_board;
    place_piece_test "Place B in col 2 in partially played board" 'B' 2
      c2 test_board;
    place_piece_test "Place R in a filled col 1" 'R' 1 fail_c
      filled_col_board;
    place_piece_test "Place B in a nonexisting col" 'B' 0 fail_c
      test_board;
    place_piece_test "Place R in a nonexisting col" 'R' 8 fail_c
      test_board;
  ]

let connect_is_winner_test name lst exp =
  name >:: fun _ -> assert_equal exp (is_winner lst)

let connect_matched t =
  match t with
  | Legal t -> t
  | Illegal -> init_board

let b_win =
  place_piece 'B' 3 test_board
  |> connect_matched |> place_piece 'R' 4 |> connect_matched
  |> place_piece 'B' 4 |> connect_matched |> place_piece 'R' 4
  |> connect_matched |> place_piece 'B' 4

let r_win =
  place_piece 'B' 2 filled_col_board
  |> connect_matched |> place_piece 'R' 4 |> connect_matched
  |> place_piece 'B' 7 |> connect_matched |> place_piece 'R' 5

let connect_is_winner_tests =
  [
    connect_is_winner_test "Empty board" init_board Nil;
    connect_is_winner_test "Not a winning state" test_board Nil;
    connect_is_winner_test "B wins" (connect_matched b_win) B;
    connect_is_winner_test "R wins" (connect_matched r_win) R;
  ]

let connect_four_tests =
  connect_board_state_tests @ connect_next_player_tests
  @ connect_player_match_string_tests @ connect_player_match_char_tests
  @ connect_plays_tests @ place_piece_tests @ connect_is_winner_tests

(* ************ END OF CONNECT 4 TESTS ************ *)

let suite =
  "test suite for Final Project"
  >::: List.flatten [ ttt_tests; hangman_tests; connect_four_tests ]

let _ = run_test_tt_main suite
