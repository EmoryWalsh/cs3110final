type square =
  | Played of char
  | Nil of int

type t = square list list

let init_board =
  [
    [ Nil 1; Nil 2; Nil 3 ];
    [ Nil 4; Nil 5; Nil 6 ];
    [ Nil 7; Nil 8; Nil 9 ];
  ]

(* type players = | 'X' | 'O' *)

(* let player = 'X' *)

type result =
  | Legal of t
  | Illegal

(* let rec print_board board = match board with | [ r1; _; _ ] -> begin
   match r1 with | [ e1; e2; e3 ] -> print_endline (e1 ^ "|" ^ e2 ^ "|"
   ^ e3) | _ -> failwith "Error: board format incorrect" end | _ ->
   failwith "Error: board format incorrect" *)
