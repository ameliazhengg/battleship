open Final_proj_3110.Ships

let welcome () =
  print_newline ();
  let () = print_string "Please enter your name: " in
  let the_input = read_line () in
  print_endline ("Welcome to Battleship, " ^ the_input)

(** Define constants *)

let column_labels = [| "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J" |]
let row_labels = [| "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10" |]

(** Function to print a single row *)
let print_row () =
  print_string "  ";
  for _ = 1 to 10 do
    print_string "|   "
  done;
  print_string "|\n";
  print_endline "\n     -----------------------------------------"

(** Function to print column labels *)
let print_column_labels () =
  print_string "     ";
  Array.iter (fun label -> print_string (label ^ "   ")) column_labels;
  print_endline ""

(** Function to print the entire grid *)
let print_grid () =
  print_column_labels ();
  print_endline "\n     -----------------------------------------";

  for i = 1 to 10 do
    print_string (row_labels.(i - 1) ^ " ");
    print_row ()
  done

(** [is_valid_row_input] requires [input] to be None or Some. Returns: true if
    input is an integer within 1-10, and false otherwise. *)
let is_valid_row_input input =
  let row_num = int_of_string_opt input in
  match row_num with
  | Some num -> num >= 1 && num <= 10
  | None -> false

(** [is_valid_col_input] requires [input] to be None or Some. Returns: true if
    input is a uppercase string between A-J, and false otherwise. *)
let is_valid_col_input input =
  match String.length input with
  | 1 ->
      let char_code = Char.code (String.get input 0) in
      char_code >= Char.code 'A' && char_code <= Char.code 'J'
  | _ -> false

let create_coord_array coordinates row_input col_input row2_input col2_input =
  if int_of_string row_input = int_of_string row2_input then
    (* horizontally placed *)
    let cols =
      let start_col = int_of_char col_input.[0] in
      let end_col = int_of_char col2_input.[0] in
      List.init
        (end_col - start_col + 1)
        (fun i ->
          ( Char.escaped (Char.uppercase_ascii (char_of_int (start_col + i))),
            row_input ))
    in
    List.fold_left
      (fun coords (col, row) -> coord_add coords row col)
      coordinates cols
  else
    (* vertically placed *)
    let rows =
      let start_row = int_of_string row_input in
      let end_row = int_of_string row2_input in
      List.init
        (end_row - start_row + 1)
        (fun _ ->
          (row_input, Char.escaped (Char.uppercase_ascii col_input.[0])))
    in
    List.fold_left
      (fun coords (row, col) -> coord_add coords row col)
      coordinates rows

(** [get_coords] prompts user for row and column value. Checks if coordinate is
    valid. If invalid, reprompts the user for a new coordiante. Otherwise,
    prints coordinate. *)
let get_coords coordinates =
  let rec first_row_coord () =
    print_endline "Enter the first coordinate of your ship: ";
    print_string "Row number (1-10): ";
    let row_input = read_line () in
    if is_valid_row_input row_input then row_input
    else begin
      print_endline "Invalid row input. Please enter a valid row number.";
      first_row_coord ()
    end
  in

  let rec first_col_coord () =
    print_string "Column letter (A-J): ";
    let col_input = read_line () in
    if is_valid_col_input col_input then col_input
    else begin
      print_endline "Invalid column input. Please enter a valid column letter.";
      first_col_coord ()
    end
  in

  let rec last_row_coord () =
    print_endline "Enter the last coordinate of your ship: ";
    print_string "Row number (1-10): ";
    let row_input = read_line () in
    if is_valid_row_input row_input then row_input
    else begin
      print_endline "Invalid row input. Please enter a valid row number.";
      last_row_coord ()
    end
  in

  let rec last_col_coord () =
    print_string "Column letter (A-J): ";
    let col_input = read_line () in
    if is_valid_col_input col_input then col_input
    else begin
      print_endline "Invalid column input. Please enter a valid column letter.";
      last_col_coord ()
    end
  in

  let row_input = first_row_coord () in
  let col_input = first_col_coord () in
  let row2_input = last_row_coord () in
  let col2_input = last_col_coord () in

  (*if its just the two space ship then add the two coordinates to the ship*)
  create_coord_array coordinates row_input col_input row2_input col2_input

let print_ship_coordinates coordinates =
  print_endline "Ship Coordinates:";
  List.iter
    (fun (row, col) -> print_endline ("(" ^ row ^ ", " ^ col ^ ")"))
    coordinates

(** Starts game and asks for name *)
let () =
  welcome ();
  print_endline "Your Battleship Board";
  print_grid ();
  print_endline "Now please choose the coordinates of your two ships. ";
  print_newline ();
  let two_coord = get_coords [] in
  let two_ship = create_ship "two ship" 2 two_coord in
  let three_coord_1 = get_coords [] in
  let three_ship_1 = create_ship "three ship" 3 three_coord_1 in
  let three_coord_2 = get_coords [] in
  let three_ship_2 = create_ship "three ship" 3 three_coord_2 in
  let four_coord = get_coords [] in
  let four_ship = create_ship "four ship" 4 four_coord in
  let five_coord = get_coords [] in
  let five_ship = create_ship "four ship" 5 five_coord in
  print_ship_coordinates two_ship.coordinates;
  print_ship_coordinates three_ship_1.coordinates;
  print_ship_coordinates three_ship_2.coordinates;
  print_ship_coordinates four_ship.coordinates;
  print_ship_coordinates five_ship.coordinates;
  print_endline "Thanks for playing. Goodbye!"
