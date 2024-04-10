type board = string array array

let column_labels = [| 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' |]

let row_labels =
  [| "  1"; "  2"; "  3"; "  4"; "  5"; "  6"; "  7"; "  8"; "  9"; " 10" |]

let create_user_board () = Array.make_matrix 10 10 "   "

let get_first_element lst index =
  let tuple = List.nth lst index in
  fst tuple

let get_second_element lst index =
  let tuple = List.nth lst index in
  snd tuple

let match_ship n =
  match n with
  | 2 -> " a "
  | 31 -> " b "
  | 32 -> " c "
  | 4 -> " d "
  | 5 -> " e "
  | _ -> "  "

let set_board board lst num =
  for i = 0 to List.length lst - 1 do
    let icon = match_ship num in
    let row = get_first_element lst i in
    let col = get_second_element lst i in
    board.(row - 1).(col - 65) <- icon
  done

let get_board_element board row col = board.(row).(col)
