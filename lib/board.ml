open Ships

type board = string array array

let column_labels = [| 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' |]

let row_labels =
  [| "  1"; "  2"; "  3"; "  4"; "  5"; "  6"; "  7"; "  8"; "  9"; " 10" |]

(* create the board size *)
let create_user_board () = Array.make_matrix 10 10 "   "

(** check_orientation chekcs if the orientation is allowed given the coordinate
    the user has inputted *)
let check_orientation orientation coord num =
  if orientation coord num < 0 || orientation coord num > 10 then false
  else true

(** checks if the coordinates of the ship are coordinates of a ship that has
    already been initiated *)
let check_ships_coord all_ship_coords coords ship_coords =
  (* if the potential ship coordinates are not in the list of all ship
     coordinates then add them and create the array representing the ships
     coordinates as well*)
  if List.for_all (fun coord -> not (List.mem coord all_ship_coords)) coords
  then
    (* add them to the list and return true *)
    (let new_ship_coords = coord_add ship_coords coords in
     fun (x, y) -> (x, y) :: all_ship_coords)
      coords new_ship_coords
  else
    (* If any coordinate is already in the list, return false *)
    (None, fun _ -> all_ship_coords)

(** Creates a list of coordinates of the ship *)
let create_coord_array ship_coords orientation row_input col_input nums
    all_ship_coords =
  let rec generate_coords acc n =
    if n < 0 then acc
    else
      match orientation with
      | "left" ->
          let coord = (char_of_int (int_of_char col_input - n), row_input) in
          generate_coords (coord :: acc) (n - 1)
      | "right" ->
          let coord = (char_of_int (int_of_char col_input + n), row_input) in
          generate_coords (coord :: acc) (n - 1)
      | "up" ->
          let row = row_input + n in
          let coord = (col_input, row) in
          generate_coords (coord :: acc) (n - 1)
      | "down" ->
          let row = row_input - n in
          let coord = (col_input, row) in
          generate_coords (coord :: acc) (n - 1)
      | _ -> failwith "Invalid orientation"
  in
  let coordinates = generate_coords [] nums in
  check_ships_coord all_ship_coords coordinates ship_coords
(** all ship coords - coords of all the ships in the board, coordiantes = the
    potential coordinates of the new ship, ship_coords is empty until the next
    function that gets filled*)

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
    board.(row - 1).(col - 65) <- icon
  done

(* gets the element at the specific row and col in our board*)
let get_board_element board row col = board.(row).(col)
