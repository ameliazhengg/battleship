open Ships

type board = string array array
type coord_list = (int * int) list

let column_labels = [| 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' |]

let row_labels =
  [| "  1"; "  2"; "  3"; "  4"; "  5"; "  6"; "  7"; "  8"; "  9"; " 10" |]

let user_ship_coords = ref []
let board_array board = board

(* create the board size *)
let create_board _ = Array.make_matrix 10 10 "   "

(* gets the element at the specific row and col in our board*)
let get_board_element board row col = board.(row).(col)

(* gets the first element in a tuple in our lists of coordinates ie the row
   number*)
let get_first_element (lst : coord_list) index =
  let tuple = List.nth lst index in
  fst tuple

(* gets the second element in a tuple in our lists of cooridnates ie the column
   letter*)
let get_second_element lst index =
  let tuple = List.nth lst index in
  snd tuple

(* add coordinates to user_ship_coords list *)
let add_coords coords = user_ship_coords := coords @ !user_ship_coords

(* Check if a guess is valid meaning check if the guess is in the ship
   coordinates *)
let valid_guess row col = not (List.mem (row, col) !user_ship_coords)

(* icon representing the different ships on our board*)
let match_ship n =
  match n with
  | 2 -> " a "
  | 3 -> " b "
  | 6 -> " c "
  | 4 -> " d "
  | 5 -> " e "
  | _ -> "   "

(* sets the positions of the user ships*)
let set_board board lst name_ship length_ship =
  let n = if name_ship = 31 then 6 else length_ship in
  for i = 0 to List.length lst - 1 do
    let icon = match_ship n in
    let row = get_first_element lst i in
    let col = get_second_element lst i in
    board.(row - 1).(col - 1) <- icon
  done

(** check_orientation checks if the orientation is allowed given the coordinate
    the user has inputed *)
let check_orientation orientation coord num =
  if orientation coord num < 0 || orientation coord num > 11 then false
  else true

(** checks if the coordinates of the ship are coordinates of a ship that has
    already been initiated *)
let check_ships_coord board lst name_ship length_ship =
  let rec check_coords lst =
    match lst with
    | [] -> true (* All coordinates are empty, return true *)
    | (row, col) :: rest ->
        if valid_guess row col then check_coords rest
        else false (* Coordinate already occupied, return false *)
  in
  let all_empty = check_coords lst in
  if all_empty then begin
    List.iter (fun (a, b) -> Printf.printf "(%d, %d)\n" a b) lst;
    (*get rid of later*)
    add_coords lst;
    (* add to full list of coordinates*)
    set_board board lst name_ship length_ship;
    (* sets the board with the coordinates*)
    add_user_ship name_ship length_ship lst;
    (* creates a ship item that contains the list of coordinates for the
       specific given ship*)
    true
  end
  else false

(** Creates a list of coordinates of the ship *)
let create_coord_array orientation row_input col_input name_ship length_ship
    board =
  let rec generate_coords acc n =
    if n <= 0 then acc
    else
      match orientation with
      | "left" ->
          let col = col_input - n + 1 in
          let coord = (row_input, col) in
          generate_coords (coord :: acc) (n - 1)
      | "right" ->
          let col = col_input + n - 1 in
          let coord = (row_input, col) in
          generate_coords (coord :: acc) (n - 1)
      | "up" ->
          let row = row_input - n + 1 in
          let coord = (row, col_input) in
          generate_coords (coord :: acc) (n - 1)
      | "down" ->
          let row = row_input + n - 1 in
          let coord = (row, col_input) in
          generate_coords (coord :: acc) (n - 1)
      | _ -> failwith "Invalid orientation"
  in
  let coordinates = generate_coords [] length_ship in
  check_ships_coord board coordinates name_ship length_ship

(** all ship coords - coords of all the ships in the board, coordiantes = the
    potential coordinates of the new ship, ship_coords is empty until the next
    function that gets filled*)
