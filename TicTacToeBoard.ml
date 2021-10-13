type square =
  | X
  | O
  | Nil of int

type t = square list list

let start_board =
  [
    [ Nil 1; Nil 2; Nil 3 ];
    [ Nil 4; Nil 5; Nil 6 ];
    [ Nil 7; Nil 8; Nil 9 ];
  ]

let player = X

let rec print_board [ r1; r2; r3 ] =
  match [ r1; r2; r3 ] with
  | [ e1; e2; e3 ] -> print_endline (e1 ^ "|" ^ e2 ^ "|" ^ e3)
  | _ -> failwith "Error: board format incorrect"