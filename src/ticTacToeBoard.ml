type players =
  | X
  | O

type square =
  | Played of players
  | Nil of int

type t = square list list

exception BoardFomat

let init_board =
  [
    [ Nil 1; Nil 2; Nil 3 ];
    [ Nil 4; Nil 5; Nil 6 ];
    [ Nil 7; Nil 8; Nil 9 ];
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

let row_state row =
  match row with
  | [ e1; e2; e3 ] -> (
      let lst_mapped = List.map piece_match row in
      match lst_mapped with
      | [ e1; e2; e3 ] -> e1 ^ " | " ^ e2 ^ " | " ^ e3 ^ "\n"
      | _ -> raise BoardFomat)
  | _ -> raise BoardFomat

let rec board_state board =
  match board with
  | [ r1; r2; r3 ] ->
      begin
        " " ^ row_state r1 ^ "___|___|___\n" ^ " " ^ row_state r2
        ^ "___|___|___\n" ^ " " ^ row_state r3 ^ "   |   |   \n"
      end
  | _ -> raise BoardFomat
