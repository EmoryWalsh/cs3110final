type players =
  | X
  | O

type square =
  | Played of (players * int)
  | Nil of int

type t = square list list

exception MalformedBoard

exception InvalidIndex of int

let init_board =
  [
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
  x : int list;
  o : int list;
  nil : int list;
}

(** [list_os s] creates a list with the indices of each square that is
    an O. *)
let list_os state =
  let lst = List.flatten state in
  let filtered =
    List.filter
      (fun x ->
        match x with
        | Played p -> (
            match p with
            | O, _ -> true
            | X, _ -> false)
        | Nil i -> false)
      lst
  in
  List.map
    (fun x ->
      match x with
      | Played (O, i) -> i
      | _ -> raise MalformedBoard)
    filtered

(** [list_xs s] creates a list with the indices of each square that is
    an X. *)
let list_xs state =
  let lst = List.flatten state in
  let filtered =
    List.filter
      (fun x ->
        match x with
        | Played p -> (
            match p with
            | O, _ -> false
            | X, _ -> true)
        | Nil i -> false)
      lst
  in
  List.map
    (fun x ->
      match x with
      | Played (X, i) -> i
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
  { x = list_xs state; o = list_os state; nil = list_nils state }

let piece_match square =
  match square with
  | Played (X, _) -> "X"
  | Played (O, _) -> "O"
  | Nil i -> string_of_int i

let player_match p =
  match p with
  | X -> 'X'
  | O -> 'O'

(** Creates the string representation of a row of a connect4 board. *)
let row_state row =
  match row with
  | [ e1; e2; e3; e4; e5; e6; e7 ] -> (
      let lst_mapped = List.map piece_match row in
      match lst_mapped with
      | [ e1; e2; e3; e4; e5; e6; e7 ] ->
          e1 ^ " | " ^ e2 ^ " | " ^ e3 ^ " | " ^ e4 ^ " | " ^ e5 ^ " | "
          ^ e6 ^ " | " ^ e7 ^ "\n"
      | _ -> raise MalformedBoard)
  | _ -> raise MalformedBoard

(** [find_square i state] is the square at index [i] in [state]. Raises
    [MalformedBoard] if the board is formatted incorrectly. Raises
    [InvalidIndex i] if the index is out of bounds. *)
let find_square i (state : t) =
  match state with
  | [
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

(** [is_open i state] is a bool stating whether the square labeled [i]
    is open or not. *)
let is_open column state =
  if column = 1 then
    let square = find_square 6 state in
    match square with
    | Played p -> false
    | Nil int -> true
  else if column = 2 then
    let square = find_square 12 state in
    match square with
    | Played p -> false
    | Nil int -> true
  else if column = 3 then
    let square = find_square 18 state in
    match square with
    | Played p -> false
    | Nil int -> true
  else if column = 4 then
    let square = find_square 24 state in
    match square with
    | Played p -> false
    | Nil int -> true
  else if column = 5 then
    let square = find_square 30 state in
    match square with
    | Played p -> false
    | Nil int -> true
  else if column = 6 then
    let square = find_square 36 state in
    match square with
    | Played p -> false
    | Nil int -> true
  else if column = 7 then
    let square = find_square 42 state in
    match square with
    | Played p -> false
    | Nil int -> true
  else raise (InvalidIndex column)

let next_player p =
  match p with
  | X -> O
  | O -> X

let char_to_player c =
  match c with
  | 'X' -> X
  | 'O' -> O
  | _ -> raise MalformedBoard

let get_column i =
  if i >= 1 && i <= 6 then 1
  else if i >= 7 && i <= 12 then 2
  else if i >= 13 && i <= 18 then 3
  else if i >= 19 && i <= 24 then 4
  else if i >= 25 && i <= 30 then 5
  else if i >= 31 && i <= 36 then 6
  else if i >= 37 && i <= 42 then 7
  else raise (InvalidIndex i)

let place_piece (player : char) i state =
  try
    if is_open i state then
      let p_player = char_to_player player in
      let square = find_square i state in
      let mapped =
        List.map
          (fun row ->
            List.map
              (fun el ->
                if el = square then Played (p_player, i) else el)
              row)
          state
      in
      Legal mapped
    else Illegal
  with
  | _ -> Illegal
