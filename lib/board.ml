type board = string array array

let column_labels = [| 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' |]

let row_labels =
  [| "  1"; "  2"; "  3"; "  4"; "  5"; "  6"; "  7"; "  8"; "  9"; " 10" |]

(* create the board size *)
let create_user_board () = Array.make_matrix 10 10 "   "

(* gets the element at the specific row and col in our board*)
let get_board_element board row col = board.(row).(col)

(* gets the first element in a tuple in our lists of coordinates ie the row
   number*)
let get_first_element lst index =
  let tuple = List.nth lst index in
  fst tuple

(* gets the second element in a tuple in our lists of cooridnates ie the column
   letter*)
let get_second_element lst index =
  let tuple = List.nth lst index in
  snd tuple

(* icon representing the different ships on our board*)
let match_ship n =
  match n with
  | 2 -> " a "
  | 31 -> " b "
  | 32 -> " c "
  | 4 -> " d "
  | 5 -> " e "
  | _ -> "  "

(* sets the positions of the user ships*)

let set_board board lst num =
  for i = 0 to List.length lst - 1 do
    let icon = match_ship num in
    let row = get_first_element lst i in
    let col = get_second_element lst i in
    board.(row).(col) <- icon
  done

(** check_orientation chekcs if the orientation is allowed given the coordinate
    the user has inputted *)
let check_orientation orientation coord num =
  if orientation coord num < 0 || orientation coord num > 10 then false
  else true

(** checks if the coordinates of the ship are coordinates of a ship that has
    already been initiated *)
let check_ships_coord board lst nums =
  let rec check_coords lst =
    match lst with
    | [] -> true (* All coordinates are empty, return true *)
    | (row, col) :: rest ->
        if get_board_element board row col = "   " then check_coords rest
        else false (* Coordinate already occupied, return false *)
  in
  let all_empty = check_coords lst in
  if all_empty then begin
    set_board board lst nums;
    true
  end
  else false

(** Creates a list of coordinates of the ship *)
let create_coord_array orientation row_input col_input nums board =
  let rec generate_coords acc n =
    if n < 0 then acc
    else
      match orientation with
      | "left" ->
          let col = col_input - n in
          (* Adjusting column index directly *)
          let coord = (row_input, col) in
          generate_coords (coord :: acc) (n - 1)
      | "right" ->
          let col = col_input + n in
          (* Adjusting column index directly *)
          let coord = (row_input, col) in
          generate_coords (coord :: acc) (n - 1)
      | "up" ->
          let row = row_input + n in
          let coord = (row, col_input) in
          generate_coords (coord :: acc) (n - 1)
      | "down" ->
          let row = row_input - n in
          let coord = (row, col_input) in
          generate_coords (coord :: acc) (n - 1)
      | _ -> failwith "Invalid orientation"
  in
  let coordinates = generate_coords [] nums in
  check_ships_coord board coordinates nums

(** all ship coords - coords of all the ships in the board, coordiantes = the
    potential coordinates of the new ship, ship_coords is empty until the next
    function that gets filled*)
