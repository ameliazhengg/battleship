open Final_proj_3110.Board
open Final_proj_3110.Logic
open Final_proj_3110.Computer

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

(** [get_coords] prompts user for row and column value. Checks if coordinate is
    valid. If invalid, reprompts the user for a new coordiante. Otherwise,adds
    coord to a list . *)
let rec get_coords name_ship board =
  let length_ship = if name_ship = 31 then 3 else name_ship in
  let rec first_row_coord () =
    print_endline
      ("Enter the first coordinate of your " ^ string_of_int length_ship
     ^ " ship: ");
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
  let row_input = int_of_string (first_row_coord ()) in
  let col_input = first_col_coord () in

  let rec get_orientation () =
    print_string "Orientation (up, down, left, right): ";
    let user_input = read_line () in
    match user_input with
    | "left" ->
        if check_orientation ( - ) (int_of_char col_input.[0] - 65) length_ship
        then "left"
        else begin
          print_endline "This orientation doesn't work with your coordinates.";
          get_orientation ()
        end
    | "right" ->
        if check_orientation ( + ) (int_of_char col_input.[0] - 65) length_ship
        then "right"
        else begin
          print_endline "This orientation doesn't work with your coordinates.";
          get_orientation ()
        end
    | "up" ->
        if check_orientation ( - ) row_input length_ship then "up"
        else begin
          print_endline "This orientation doesn't work with your coordinates.";
          get_orientation ()
        end
    | "down" ->
        if check_orientation ( + ) row_input length_ship then "down"
        else begin
          print_endline "This orientation doesn't work with your coordinates.";
          get_orientation ()
        end
    | _ ->
        print_endline
          "Invalid orientation. Please enter 'up', 'down', 'left', or 'right'.";
        get_orientation ()
  in
  let orientation = get_orientation () in

  match
    create_coord_array orientation row_input
      (int_of_char col_input.[0] - 65)
      name_ship length_ship board
    (* nums is the name of the ship 1,2,3,31,4,5 and n is just the length of
       ship*)
  with
  | true -> ()
  | false ->
      print_endline "Cannot add coordinates due to overlap";
      get_coords name_ship board

(** Starts game and asks for name *)
let () =
  welcome ();
  print_endline "Computer Board";
  print_endline string_comp_ships;
  print_endline string_occ_coord;
  print_endline string_row_col;
  add_coords;
  let computer_board = create_computer_board () in
  print_grid (random_board computer_board);
  print_endline "Your Battleship Board";
  let user_board = create_board () in
  print_endline "";
  (* user_board; *)
  print_endline "Now please choose the coordinates of your ships ";
  print_newline ();
  get_coords 2 user_board;
  get_coords 3 user_board;
  get_coords 31 user_board;
  (* had to do this because there are two ships of length 3 *)
  get_coords 4 user_board;
  get_coords 5 user_board;
  print_grid user_board;
  print_newline ();
  print_endline "Thanks for playing. Goodbye!"
