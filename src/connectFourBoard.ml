type players =
  | B
  | R

type square =
  | Played of (players * int)
  | Nil of int

type t = square list list

exception MalformedBoard

exception InvalidIndex of int
let init_board =
  [
    [ Nil 1; Nil 2; Nil 3; Nil 4; Nil 5; Nil 6; Nil 7 ];
    [ Nil 6; Nil 12; Nil 18; Nil 24; Nil 30; Nil 36; Nil 42 ];
    [ Nil 5; Nil 11; Nil 17; Nil 23; Nil 29; Nil 35; Nil 41 ];
    [ Nil 4; Nil 10; Nil 16; Nil 22; Nil 28; Nil 34; Nil 40 ];
    [ Nil 3; Nil 9; Nil 15; Nil 21; Nil 27; Nil 33; Nil 39 ];
    [ Nil 2; Nil 8; Nil 14; Nil 20; Nil 26; Nil 32; Nil 38 ];
    [ Nil 1; Nil 7; Nil 13; Nil 19; Nil 25; Nil 31; Nil 37 ];
  ]

type result =
  | Legal of t
  | Illegal

type plays = {
  b : int list;
  r : int list;
  nil : int list;
}

(** [list_rs s] creates a list with the indices of each square that is
    an R. *)
let list_rs state =
  let lst = List.flatten state in
  let filtered =
    List.filter
      (fun x ->
        match x with
        | Played p -> (
            match p with
            | R, _ -> true
            | B, _ -> false)
        | Nil i -> false)
      lst
  in
  List.map
    (fun x ->
      match x with
      | Played (R, i) -> i
      | _ -> raise MalformedBoard)
    filtered

(** [list_bs s] creates a list with the indices of each square that is
    a B. *)
let list_bs state =
  let lst = List.flatten state in
  let filtered =
    List.filter
      (fun x ->
        match x with
        | Played p -> (
            match p with
            | R, _ -> false
            | B, _ -> true)
        | Nil i -> false)
      lst
  in
  List.map
    (fun x ->
      match x with
      | Played (B, i) -> i
      | _ -> raise MalformedBoard)
    filtered

(** [list_nils s] creates a list with the indices of each square that is
    Nil. *)
let list_nils state =
  let lst = List.flatten state in
  let filtered =
    List.filter
      (fun x ->
        match x with
        | Played _ -> false
        | Nil i -> true)
      lst
  in
  List.map
    (fun x ->
      match x with
      | Nil x -> x
      | Played p -> raise MalformedBoard)
    filtered

let plays state =
  { b = list_bs state; r = list_rs state; nil = list_nils state }

(** [piece_match square] matches the contents of a square to a string. *)
let piece_match square =
  match square with
  | Played (B, _) -> "B"
  | Played (R, _) -> "R"
  | Nil i -> ""
  

let player_match_string p =
  match p with
  | B -> "Blue"
  | R -> "Red"
  let player_match_char p =
    match p with
    | B -> 'B'
    | R -> 'R'

(** Creates the string representation of a row of a connect4 board. *)
let top_row_state row =
  match row with
  | [ e1; e2; e3; e4; e5; e6; e7 ] -> 
          "1" ^ " | " ^ "2" ^ " | " ^ "3" ^ " | " ^ "4" ^ " | " ^ "5" ^ " | "
          ^ "6" ^ " | " ^ "7" ^ "\n"
  | _ -> raise MalformedBoard

let row_state row = match row with | [ e1; e2; e3; e4; e5; e6; e7 ] -> (
  let lst_mapped = List.map piece_match row in
  match lst_mapped with
  | [ e1; e2; e3; e4; e5; e6; e7 ] ->
      e1 ^  "  | " ^ e2 ^ "  | " ^ e3 ^ "  | " ^ e4 ^ "  | " ^ e5 ^ "  | "
      ^ e6 ^ "  | " ^ e7 ^ "\n"
  | _ -> raise MalformedBoard)
| _ -> raise MalformedBoard
let board_state board =
  match board with
  | [ r1; r2; r3; r4; r5; r6; r7;] ->
      begin
        " " ^ top_row_state r1 ^ " " ^ row_state r2
        ^ "___|___|___|___|___|___|___\n" ^ " " ^ row_state r3 ^ "___|___|___|___|___|___|___\n" ^ " " ^ row_state r4
        ^ "___|___|___|___|___|___|___\n" ^" " ^ row_state r5
        ^ "___|___|___|___|___|___|___\n" ^" " ^ row_state r6
        ^ "___|___|___|___|___|___|___\n" ^" " ^ row_state r7
        ^ "   |   |   |   |   |   |     \n"
      end
  | _ -> raise MalformedBoard

(** [find_square i state] is the square at index [i] in [state]. Raises
    [MalformedBoard] if the board is formatted incorrectly. Raises
    [InvalidIndex i] if the index is out of bounds. *)
let find_square i (state : t) =
  match state with
  | [[ c1; c2; c3; c4; c5; c6; c7 ];
   [ e6; e12; e18; e24; e30; e36; e42 ];
   [ e5; e11; e17; e23; e29; e35; e41 ];
   [ e4; e10; e16; e22; e28; e34; e40 ];
   [ e3; e9; e15; e21; e27; e33; e39 ];
   [ e2; e8; e14; e20; e26; e32; e38 ];
   [ e1; e7; e13; e19; e25; e31; e37 ];
  ] ->
      if i = 1 then e1
      else if i = 2 then e2
      else if i = 3 then e3
      else if i = 4 then e4
      else if i = 5 then e5
      else if i = 6 then e6
      else if i = 7 then e7
      else if i = 8 then e8
      else if i = 9 then e9
      else if i = 10 then e10
      else if i = 11 then e11
      else if i = 12 then e12
      else if i = 13 then e13
      else if i = 14 then e14
      else if i = 15 then e15
      else if i = 16 then e16
      else if i = 17 then e17
      else if i = 18 then e18
      else if i = 19 then e19
      else if i = 20 then e20
      else if i = 21 then e21
      else if i = 22 then e22
      else if i = 23 then e23
      else if i = 24 then e24
      else if i = 25 then e25
      else if i = 26 then e26
      else if i = 27 then e27
      else if i = 28 then e28
      else if i = 29 then e29
      else if i = 30 then e30
      else if i = 31 then e31
      else if i = 32 then e32
      else if i = 33 then e33
      else if i = 34 then e34
      else if i = 35 then e35
      else if i = 36 then e36
      else if i = 37 then e37
      else if i = 38 then e38
      else if i = 39 then e39
      else if i = 40 then e40
      else if i = 41 then e41
      else if i = 42 then e42
      else raise (InvalidIndex i)
  | _ -> raise MalformedBoard
let square_is_open square =
  match square with
  | Nil s -> true
  | Played p -> false
(** [is_column_open column state] is a bool stating whether the column labeled
    [column] is open or not. *)
let is_column_open column state =
  if column = 1 then
    let square = find_square 6 state in
    square_is_open square
  else if column = 2 then
    let square = find_square 12 state in
    square_is_open square
  else if column = 3 then
    let square = find_square 18 state in
    square_is_open square
  else if column = 4 then
    let square = find_square 24 state in
    square_is_open square
  else if column = 5 then
    let square = find_square 30 state in
    square_is_open square
  else if column = 6 then
    let square = find_square 36 state in
    square_is_open square
  else if column = 7 then
    let square = find_square 42 state in
    square_is_open square
  else raise (InvalidIndex column)
let next_place_in_column col state =
  if col = 1 then 
    (if square_is_open(find_square 1 state) then 1 
    else if square_is_open(find_square 2 state) then 2 
    else if square_is_open(find_square 3 state) then 3 
    else if square_is_open(find_square 4 state) then 4 
    else if square_is_open(find_square 5 state) then 5 
    else 6) 
  else if col = 2 then 
    (if square_is_open(find_square 7 state) then 7 
    else if square_is_open(find_square 8 state) then 8 
    else if square_is_open(find_square 9 state) then 9 
    else if square_is_open(find_square 10 state) then 10 
    else if square_is_open(find_square 11 state) then 11 
    else 12) 
  else if col = 3 then 
    (if square_is_open(find_square 13 state) then 13 
    else if square_is_open(find_square 14 state) then 14 
    else if square_is_open(find_square 15 state) then 15 
    else if square_is_open(find_square 16 state) then 16 
    else if square_is_open(find_square 17 state) then 17 
    else 18) 
  else if col = 4 then 
    (if square_is_open(find_square 19 state) then 19 
    else if square_is_open(find_square 20 state) then 20 
    else if square_is_open(find_square 21 state) then 21 
    else if square_is_open(find_square 22 state) then 22 
    else if square_is_open(find_square 23 state) then 23 
    else 24) 
  else if col = 5 then 
    (if square_is_open(find_square 25 state) then 25 
    else if square_is_open(find_square 26 state) then 26 
    else if square_is_open(find_square 27 state) then 27 
    else if square_is_open(find_square 28 state) then 28 
    else if square_is_open(find_square 29 state) then 29 
    else 30) 
  else if col = 6 then 
    (if square_is_open(find_square 31 state) then 31 
    else if square_is_open(find_square 32 state) then 32 
    else if square_is_open(find_square 33 state) then 33 
    else if square_is_open(find_square 34 state) then 34 
    else if square_is_open(find_square 35 state) then 35 
    else 36) 
  else if col = 7 then
    (if square_is_open(find_square 37 state) then 37 
    else if square_is_open(find_square 38 state) then 38 
    else if square_is_open(find_square 39 state) then 39 
    else if square_is_open(find_square 40 state) then 40 
    else if square_is_open(find_square 41 state) then 41 
    else 42)
  else raise (InvalidIndex col)
let next_player p =
  match p with
  | B -> R
  | R -> B

let char_to_player c =
  match c with
  | 'B' -> B
  | 'R' -> R
  | _ -> raise MalformedBoard

(** [place_piece player i state] is the new state representation when
    [player] adds a piece to board [state] at column [col]. Raises
    [InvalidIndex i] when [i] is out of bounds or already played. *)
let place_piece (player : char) col state =
  try
    if is_column_open col state then
      let p_player = char_to_player player in
      let next_opening = next_place_in_column col state in
      let square1 = find_square next_opening state in
      let mapped =
        List.map
          (fun row ->
            List.map
              (fun el ->
                if el = square1 then Played (p_player, next_opening) else el)
              row)
          state
      in
      Legal mapped
    else
      Illegal
  with
  | _ -> Illegal

type winner = 
  | B
  | R
  | Tie
  | Nil

(** [is_subset wins lst] returns whether [lst] is a subset of [wins].*)
let is_subset (wins : int list list) (lst : int list) =
  let tf_lst =
    List.map
      (fun row ->
        match row with
        | [ e1; e2; e3; e4] ->
            if
              List.exists (fun x -> x = e1) lst
              && List.exists (fun x -> x = e2) lst
              && List.exists (fun x -> x = e3) lst
              && List.exists (fun x -> x = e4) lst
            then true
            else false
        | _ -> raise MalformedBoard)
      wins
  in
  List.fold_left (fun acc row -> acc || row) false tf_lst
  let is_winning_state lst =
    let wins =
      [
        [1; 2; 3; 4];
        [7; 8; 9; 10];
        [13; 14; 15; 16];
        [19; 20; 21; 22];
        [25; 26; 27; 28];
        [31; 32; 33; 34];
        [37; 38; 39; 40];
        [2; 3; 4; 5];
        [8; 9; 10; 11];
        [14; 15; 16; 17];
        [20; 21; 22; 23];
        [26; 27; 28; 29];
        [32; 33; 34; 35];
        [38; 39; 40; 41];
        [3; 4; 5; 6];
        [9; 10; 11; 12];
        [15; 16; 17; 18];
        [21; 22; 23; 24];
        [27; 28; 29; 30];
        [33; 34; 35; 36];
        [39; 40; 41; 42];
        [1; 7; 13; 19];
        [7; 13; 19; 25];
        [13; 19; 25; 31];
        [19; 25; 31; 37];
        [2; 8; 14; 20];
        [8; 14; 20; 26];
        [14; 20; 26; 32];
        [20; 26; 32; 38];
        [3; 9; 15; 21];
        [9; 15; 21; 27];
        [15; 21; 27; 33];
        [21; 27; 33; 39];
        [4; 10; 16; 22];
        [10; 16; 22; 28];
        [16; 22; 28; 34];
        [22; 28; 34; 40];
        [5; 11; 17; 23];
        [11; 17; 23; 29];
        [17; 23; 29; 35];
        [23; 29; 35; 41];
        [6; 12; 18; 24];
        [12; 18; 24; 30];
        [18; 24; 30; 36];
        [24; 30; 36; 42];
        [3; 10; 17; 24];
        [2; 9; 16; 23];
        [9; 16; 23; 30];
        [1; 8; 15; 22];
        [8; 15; 22; 29];
        [15; 22; 29; 36];
        [7; 14; 21; 28];
        [14; 21; 28; 35];
        [21; 28; 35; 42];
        [13; 20; 27; 34];
        [20; 27; 34; 41];
        [19; 26; 33; 40];
        [4; 9; 14; 19];
        [5; 10; 15; 20];
        [10; 15; 20; 25];
        [6; 11; 16; 21];
        [11; 16; 21; 26];
        [16; 21; 26; 31];
        [12; 17; 22; 27];
        [17; 22; 27; 32];
        [22; 27; 32; 37];
        [18; 23; 28; 33];
        [23; 28; 33; 38];
        [24; 29; 34; 39];
      ]
    in
    is_subset wins lst

let is_winner state =
  let move_lst = plays state in
  match move_lst with
  | { b; r; nil } ->
      if is_winning_state b then B
      else if is_winning_state r then R
      else if nil = [] then Tie
      else Nil