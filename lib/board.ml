open Ships

type board = string array array
type coord_list = (int * int) list

let theme = ref 0
let column_labels = [| 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' |]

let row_labels =
  [| "  1"; "  2"; "  3"; "  4"; "  5"; "  6"; "  7"; "  8"; "  9"; " 10" |]

let user_ship_coords = ref []

let in_user_ship_coords guess =
  if List.mem guess !user_ship_coords then true else false

(* create the board size *)
let create_board t =
  theme := t - 1;
  Array.make_matrix 10 10 "   "

let themes_list =
  [
    [
      "\x1b[38;5;215m";
      "\x1b[38;5;216m";
      "\x1b[38;5;217m";
      "\x1b[38;5;218m";
      "\x1b[38;5;219m";
    ];
    (* Peach Vibe*)
    [
      "\x1b[38;5;118m";
      "\x1b[38;5;154m";
      "\x1b[38;5;226m";
      "\x1b[38;5;228m";
      "\x1b[38;5;230m";
    ];
    (* Tropical Vibe *)
    [
      "\x1b[38;5;27m";
      "\x1b[38;5;33m";
      "\x1b[38;5;75m";
      "\x1b[38;5;141m";
      "\x1b[38;5;135m";
    ];
    (* Arctic Vibe *)
    [
      "\x1b[38;5;45m";
      "\x1b[38;5;42m";
      "\x1b[38;5;148m";
      "\x1b[38;5;190m";
      "\x1b[38;5;226m";
    ];
    (* Oasis Vibe *)
    [
      "\x1b[38;5;126m";
      "\x1b[38;5;168m";
      "\x1b[38;5;210m";
      "\x1b[38;5;204m";
      "\x1b[38;5;202m";
    ];
    (* Cosmic Vibe *)
    [
      "\x1b[38;5;214m";
      "\x1b[38;5;220m";
      "\x1b[38;5;202m";
      "\x1b[38;5;226m";
      "\x1b[38;5;209m";
    ];
    (* Fantasy Vibe *)
  ]

let user_board_array board = board

(* gets the element at the specific row and col in our board*)
let get_user_board_element board row col = board.(row).(col)
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
let get_color n = List.nth (List.nth themes_list !theme) n

(* icon representing the different ships on our board*)
let match_ship n =
  match n with
  | 2 -> get_color 0 ^ " a " ^ "\x1b[0m"
  | 3 -> get_color 1 ^ " b " ^ "\x1b[0m"
  | 6 -> get_color 2 ^ " c " ^ "\x1b[0m"
  | 4 -> get_color 3 ^ " d " ^ "\x1b[0m"
  | 5 -> get_color 4 ^ " e " ^ "\x1b[0m"
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

(*generate a list of coordinates*)
let rec generate_coords acc n orientation row_input col_input =
  if n <= 0 then acc
  else
    match orientation with
    | "left" ->
        let col = col_input - n + 1 in
        let coord = (row_input, col) in
        generate_coords (coord :: acc) (n - 1) orientation row_input col_input
    | "right" ->
        let col = col_input + n - 1 in
        let coord = (row_input, col) in
        generate_coords (coord :: acc) (n - 1) orientation row_input col_input
    | "up" ->
        let row = row_input - n + 1 in
        let coord = (row, col_input) in
        generate_coords (coord :: acc) (n - 1) orientation row_input col_input
    | "down" ->
        let row = row_input + n - 1 in
        let coord = (row, col_input) in
        generate_coords (coord :: acc) (n - 1) orientation row_input col_input
    | _ -> failwith "Invalid orientation"

(** Creates a list of coordinates of the ship and checks if it is valid*)
let create_coord_array orientation row_input col_input name_ship length_ship
    board =
  let coordinates =
    generate_coords [] length_ship orientation row_input col_input
  in
  check_ships_coord board coordinates name_ship length_ship

(** all ship coords - coords of all the ships in the board, coordiantes = the
    potential coordinates of the new ship, ship_coords is empty until the next
    function that gets filled*)
