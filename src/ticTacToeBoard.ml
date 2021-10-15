type players =
  | X
  | O

type square =
  | Played of players
  | Nil of int

type t = square list list

exception MalformedBoard

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

(** [is_open i state] is a bool stating whether the square labeled [i]
    is open or not. *)
let is_open i state =
  raise (Failure "Unimplemented: TicTacToeBoard.place_piece")

let place_piece player i state =
  raise (Failure "Unimplemented: TicTacToeBoard.place_piece")
