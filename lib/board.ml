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

let create_board () = Array.make_matrix 10 10 "   "

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
let get_user_board_element board row col = board.(row).(col)
let get_board_element board row col = board.(row).(col)

let get_first_element (lst : coord_list) index =
  let tuple = List.nth lst index in
  fst tuple

let get_second_element lst index =
  let tuple = List.nth lst index in
  snd tuple

let add_coords coords = user_ship_coords := coords @ !user_ship_coords
let valid_guess row col = not (List.mem (row, col) !user_ship_coords)
let get_color n = List.nth (List.nth themes_list !theme) n

let match_ship n =
  match n with
  | 2 -> " a "
  | 3 -> " b "
  | 6 -> " c "
  | 4 -> " d "
  | 5 -> " e "
  | _ -> "   "

let set_board board lst name_ship length_ship =
  let n = if name_ship = 31 then 6 else length_ship in
  for i = 0 to List.length lst - 1 do
    let icon = get_color (n - 2) ^ match_ship n ^ "\027[0m" in
    let row = get_first_element lst i in
    let col = get_second_element lst i in
    board.(row - 1).(col - 1) <- icon
  done

let check_orientation orientation coord num =
  if orientation coord num < 0 || orientation coord num > 11 then false
  else true

let check_ships_coord board lst name_ship length_ship =
  let rec check_coords lst =
    match lst with
    | [] -> true
    | (row, col) :: rest ->
        if valid_guess row col then check_coords rest else false
  in
  let all_empty = check_coords lst in
  if all_empty then begin
    add_coords lst;
    set_board board lst name_ship length_ship;
    add_user_ship name_ship length_ship lst;

    true
  end
  else false

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

let create_coord_array orientation row_input col_input name_ship length_ship
    board =
  let coordinates =
    generate_coords [] length_ship orientation row_input col_input
  in
  check_ships_coord board coordinates name_ship length_ship
