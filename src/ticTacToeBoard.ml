type players =
  | X
  | O

type square =
  | Played of players
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
    [ Played O; Played O; Nil 3 ];
    [ Nil 4; Played X; Nil 6 ];
    [ Nil 7; Played X; Nil 9 ];
  ]

let player = X

type result =
  | Legal of t
  | Illegal

let piece_match square =
  match square with
  | Played X -> "X"
  | Played O -> "O"
  | Nil i -> string_of_int i

(** Creates the string representation of a row of a tic tac toe board. *)
let row_state row =
  match row with
  | [ e1; e2; e3 ] -> (
      let lst_mapped = List.map piece_match row in
      match lst_mapped with
      | [ e1; e2; e3 ] -> e1 ^ " | " ^ e2 ^ " | " ^ e3 ^ "\n"
      | _ -> raise MalformedBoard)
  | _ -> raise MalformedBoard

let rec board_state board =
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
  (* raise (Failure "Unimplemented: TicTacToeBoard.place_piece") *)
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

let place_piece player i state =
  raise (Failure "Unimplemented: TicTacToeBoard.place_piece")
