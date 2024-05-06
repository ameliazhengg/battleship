let () = Random.self_init ()

exception Foo of string
exception Bar of string

let columns = [| 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' |]
let rows = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
let directions = [| "up"; "down"; "left"; "right" |]

type ship = {
  name : string;
  length : int;
  coordinates : (int * char) list;
  direction : string;
}

let random_row = 1 + Random.int (Array.length rows)
let random_col = Array.get columns (1 + Random.int (Array.length columns))
let random_dir = Array.get directions (1 + Random.int (Array.length directions))
let create_computer_board () = Array.make_matrix 10 10 " "

let check_valid_placement (row : int) (col : char) (dir : string) (length : int)
    =
  if dir = "left" && int_of_char col - 64 < length then false
  else if dir = "right" && int_of_char col - 65 + length > 10 then false
  else if dir = "up" && row - length <= 0 then false
  else if dir = "down" && row + length > 10 then false
  else true

(*let fill_coordinates (row : int) (col : char) (dir : string) (length : int) =
  let ship_coords = [] in*)

(* let computer_2_ship () : ship= let row = ref random_row in let col = ref
   random_col in let dir = ref random_dir in let quit_loop = ref
   (check_valid_placement !row !col !dir 2) in while not !quit_loop do row :=
   random_row; col := random_col; dir := random_dir; quit_loop :=
   check_valid_placement !row !col !dir 2 done; {name = "2_ship"; length = 2;
   coordinates = (!row, !col); direction = !dir}*)
