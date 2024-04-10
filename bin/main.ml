open Final_proj_3110.Ships
open Final_proj_3110.Board
open Final_proj_3110.Logic

let welcome () =
  print_newline ();
  let () = print_string "Please enter your name: " in
  let the_input = read_line () in
  print_endline ("Welcome to Battleship, " ^ the_input ^ "!")

(** prints row with the battleships *)
let print_row board i =
  print_string "  ";
  for x = 0 to 9 do
    print_string "|";
    print_string (get_board_element board i x)
  done;
  print_string "|\n";
  print_endline "      -----------------------------------------"

(** print column labels *)
let print_column_labels () =
  print_string "     ";
  Array.iter
    (fun label -> print_string ("   " ^ String.make 1 label))
    column_labels;
  print_endline ""

(** print the entire grid *)
let print_grid board =
  print_column_labels ();
  print_endline "      -----------------------------------------";
  for i = 0 to 9 do
    print_string (row_labels.(i) ^ " ");
    print_row board i
  done

(** creates a list of coordinates of the ship*)
let create_coord_array coordinates row_input col_input row2_input col2_input =
  if row_input = row2_input then
    (* Horizontally placed ship *)
    let cols =
      List.init
        (col2_input - col_input + 1)
        (fun i -> (char_of_int (col_input + i), row_input))
    in
    List.fold_left
      (fun coords (col, row) -> coord_add coords row col)
      coordinates cols
  else
    (* Vertically placed ship*)
    let rows =
      List.init
        (row2_input - row_input + 1)
        (fun i -> (row_input + i, char_of_int col_input))
    in
    List.fold_left
      (fun coords (row, col) -> coord_add coords row col)
      coordinates rows

(** [get_coords] prompts user for row and column value. Checks if coordinate is
    valid. If invalid, reprompts the user for a new coordiante. Otherwise,adds
    coord to a list . *)
let get_coords nums =
  let rec first_row_coord () =
    print_endline ("Enter the first coordinate of your " ^ nums ^ " ship: ");
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
    print_endline ("Enter the last coordinate of your " ^ nums ^ " ship: ");
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

  (*create the list of coords using this function*)
  create_coord_array [] (int_of_string row_input)
    (int_of_char col_input.[0])
    (int_of_string row2_input)
    (int_of_char col2_input.[0])

(* just for printing purposes*)
let print_ship_coordinates coordinates =
  print_endline "Ship Coordinates:";
  List.iter
    (fun (row, col) ->
      print_endline ("(" ^ string_of_int row ^ ", " ^ String.make 1 col ^ ")"))
    coordinates

(** Starts game and asks for name *)
let () =
  welcome ();
  print_endline "Your Battleship Board";
  let user_board = create_user_board () in
  print_grid user_board;
  print_endline "Now please choose the coordinates of your two ships. ";
  print_newline ();
  let two_coord = get_coords "two" in
  let two_ship = create_ship "two ship" 2 two_coord in
  let three_coord_1 = get_coords "three" in
  let three_ship_1 = create_ship "three ship" 3 three_coord_1 in
  let three_coord_2 = get_coords "three" in
  let three_ship_2 = create_ship "three ship" 3 three_coord_2 in
  let four_coord = get_coords "four" in
  let four_ship = create_ship "four ship" 4 four_coord in
  let five_coord = get_coords "five" in
  let five_ship = create_ship "five ship" 5 five_coord in
  print_ship_coordinates two_ship.coordinates;
  print_ship_coordinates three_ship_1.coordinates;
  print_ship_coordinates three_ship_2.coordinates;
  print_ship_coordinates four_ship.coordinates;
  print_ship_coordinates five_ship.coordinates;
  set_board user_board (List.map (fun (x, y) -> (x, int_of_char y)) two_coord) 2;
  set_board user_board
    (List.map (fun (x, y) -> (x, int_of_char y)) three_coord_1)
    31;
  set_board user_board
    (List.map (fun (x, y) -> (x, int_of_char y)) three_coord_2)
    32;
  set_board user_board
    (List.map (fun (x, y) -> (x, int_of_char y)) four_coord)
    4;
  set_board user_board
    (List.map (fun (x, y) -> (x, int_of_char y)) five_coord)
    5;
  print_grid user_board;

  print_endline "Thanks for playing. Goodbye!"
