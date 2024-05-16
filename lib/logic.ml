let user_guesses : (int * int) list ref = ref []

(* list representing the guesses the computer has made*)
let computer_guesses : (int * int) list ref = ref []

(* adds the users recent guess to a list of guesses*)
let add_user_guess (row, col) = user_guesses := (row, col) :: !user_guesses

(* adds the computers recent guess to a list of guesses*)
let add_computer_guess guess = computer_guesses := guess :: !computer_guesses

(* check if user has already guessed this coord before*)
let valid_guess_user row col = not (List.mem (row, col) !user_guesses)

(* check if computer has already guessed this coord before*)
let valid_guess_computer guess = not (List.mem guess !computer_guesses)

(* marks the board with the symbol for hit or not hit*)
let mark_on_board board (row, col) symbol = board.(row - 1).(col - 1) <- symbol
let string_of_coord (row, col) = Printf.sprintf "%d,%c" row col

let convert_coords_to_game_format coords =
  List.map (fun (r, c) -> (r, Char.chr (c + 64))) coords

(* Convert a list of coordinates to a formatted string *)
let string_of_list_coords coords =
  let c = convert_coords_to_game_format coords in
  let strings = List.map string_of_coord c in
  String.concat "; " strings

let is_on_board_edge row_input col_input =
  match (row_input, col_input) with
  | 1, _ -> true
  | 10, _ -> true
  | _, 1 -> true
  | _, 10 -> true
  | _ -> false

let filter_valid_coords coords =
  List.filter (fun (r, c) -> valid_guess_user r c) coords

(** [is_valid_row_input] requires [input] to be None or Some. Returns: true if
    input is an integer within 1-10, and false otherwise. *)
let get_edge_coords row col =
  let directions = [ (-1, 0); (1, 0); (0, -1); (0, 1) ] in
  List.fold_left
    (fun acc (dr, dc) ->
      let new_r, new_c = (row + dr, col + dc) in
      if new_r >= 1 && new_r <= 10 && new_c >= 1 && new_c <= 10 then
        (new_r, new_c) :: acc
      else acc)
    [] directions

let get_corner_coords row col =
  match (row, col) with
  | 1, 1 -> [ (1, 2); (2, 1) ]
  | 10, 10 -> [ (10, 9); (9, 10) ]
  | 1, 10 -> [ (1, 9); (2, 10) ]
  | 10, 1 -> [ (10, 2); (9, 1) ]
  | _ -> []

let get_rec_coords row col =
  let potential_coords =
    if is_on_board_edge row col then
      let corners = [ (1, 1); (10, 10); (1, 10); (10, 1) ] in
      if List.mem (row, col) corners then get_corner_coords row col
      else get_edge_coords row col
    else get_edge_coords row col |> filter_valid_coords
  in
  if List.length potential_coords = 0 then
    (* Handle the case where there are no valid moves *)
    failwith "No recommended coordinates available"
  else potential_coords

let is_valid_row_input input =
  let row_num = int_of_string input in
  if row_num >= 1 && row_num <= 10 then true else false

(** [is_valid_col_input] requires [input] to be None or Some. Returns: true if
    input is a string between A-J case insensitive, and false otherwise. *)
let is_valid_col_input input =
  match String.length input with
  | 1 ->
      let char_code = Char.code (String.get input 0) in
      (char_code >= Char.code 'A' && char_code <= Char.code 'J')
      || (char_code >= Char.code 'a' && char_code <= Char.code 'j')
  | _ -> false
