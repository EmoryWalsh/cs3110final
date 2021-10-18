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
    [ Nil 1; Nil 2; Nil 3 ];
    [ Nil 4; Nil 5; Nil 6 ];
    [ Nil 7; Nil 8; Nil 9 ];
  ]

let test_board =
  [
    [ Played (O, 1); Played (O, 2); Nil 3 ];
    [ Nil 4; Played (X, 5); Nil 6 ];
    [ Nil 7; Played (X, 8); Nil 9 ];
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

(** [piece_match square] matches the contents of a square to a string. *)
let piece_match square =
  match square with
  | Played (X, _) -> "X"
  | Played (O, _) -> "O"
  | Nil i -> string_of_int i

let player_match p =
  match p with
  | X -> 'X'
  | O -> 'O'

(** Creates the string representation of a row of a tic tac toe board. *)
let row_state row =
  match row with
  | [ e1; e2; e3 ] -> (
      let lst_mapped = List.map piece_match row in
      match lst_mapped with
      | [ e1; e2; e3 ] -> e1 ^ " | " ^ e2 ^ " | " ^ e3 ^ "\n"
      | _ -> raise MalformedBoard)
  | _ -> raise MalformedBoard

let board_state board =
  match board with
  | [ r1; r2; r3 ] ->
      begin
        " " ^ row_state r1 ^ "___|___|___\n" ^ " " ^ row_state r2
        ^ "___|___|___\n" ^ " " ^ row_state r3 ^ "   |   |   \n"
      end
  | _ -> raise MalformedBoard

(** [find_square i state] is the square at index [i] in [state]. Raises
    [MalformedBoard] if the board is formatted incorrectly. Raises
    [InvalidIndex i] if the index is out of bounds. *)
let find_square i (state : t) =
  match state with
  | [ [ e1; e2; e3 ]; [ e4; e5; e6 ]; [ e7; e8; e9 ] ] ->
      if i = 1 then e1
      else if i = 2 then e2
      else if i = 3 then e3
      else if i = 4 then e4
      else if i = 5 then e5
      else if i = 6 then e6
      else if i = 7 then e7
      else if i = 8 then e8
      else if i = 9 then e9
      else raise (InvalidIndex i)
  | _ -> raise MalformedBoard

(** [is_open i state] is a bool stating whether the square labeled [i]
    is open or not. *)
let is_open i state =
  let square = find_square i state in
  match square with
  | Played p -> false
  | Nil int -> true

let next_player p =
  match p with
  | X -> O
  | O -> X

let char_to_player c =
  match c with
  | 'X' -> X
  | 'O' -> O
  | _ -> raise MalformedBoard

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

type winner =
  | X
  | O
  | Tie
  | Nil

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let is_subset (wins : int list list) (lst : int list) =
  let tf_lst =
    List.map
      (fun row ->
        match row with
        | [ e1; e2; e3 ] ->
            if
              List.exists (fun x -> x = e1) lst
              && List.exists (fun x -> x = e2) lst
              && List.exists (fun x -> x = e3) lst
            then true
            else false
        | _ -> raise MalformedBoard)
      wins
  in
  List.fold_left (fun acc row -> acc || row) false tf_lst

let is_winning_state lst =
  let wins =
    [
      [ 1; 2; 3 ];
      [ 4; 5; 6 ];
      [ 7; 8; 9 ];
      [ 1; 4; 7 ];
      [ 2; 5; 8 ];
      [ 3; 6; 9 ];
      [ 1; 5; 9 ];
      [ 3; 5; 7 ];
    ]
  in
  is_subset wins lst

let is_winner state =
  let move_lst = plays state in
  match move_lst with
  | { x; o; nil } ->
      if is_winning_state x then X
      else if is_winning_state o then O
      else if nil = [] then Tie
      else Nil
