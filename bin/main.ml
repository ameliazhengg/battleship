open Final_proj_3110.Board
open Final_proj_3110.Logic
open Final_proj_3110.Computer
open Final_proj_3110.Ships

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

(* gets the users input for row *)
let rec get_row_coord () =
  print_string "Enter the row number (1-10): ";
  let row_input = read_line () in
  if is_valid_row_input row_input then int_of_string row_input
  else begin
    print_endline "Invalid row input. Please enter a valid row number.";
    get_row_coord ()
  end

(*gets user input for col*)
let rec get_col_coord () =
  print_string "Enter the column letter (A-J): ";
  let col_input = read_line () in
  if is_valid_col_input col_input then col_input
  else begin
    print_endline "Invalid column input. Please enter a valid column letter.";
    get_col_coord ()
  end

(*gets user input for orientation*)
let rec get_orientation col_input row_input length_ship =
  print_string "Orientation (up, down, left, right): ";
  let user_input = read_line () in
  match user_input with
  | "left" ->
      if check_orientation ( - ) (int_of_char col_input.[0] - 65) length_ship
      then "left"
      else begin
        print_endline "This orientation doesn't work with your coordinates.";
        get_orientation col_input row_input length_ship
      end
  | "right" ->
      if check_orientation ( + ) (int_of_char col_input.[0] - 65) length_ship
      then "right"
      else begin
        print_endline "This orientation doesn't work with your coordinates.";
        get_orientation col_input row_input length_ship
      end
  | "up" ->
      if check_orientation ( - ) row_input length_ship then "up"
      else begin
        print_endline "This orientation doesn't work with your coordinates.";
        get_orientation col_input row_input length_ship
      end
  | "down" ->
      if check_orientation ( + ) row_input length_ship then "down"
      else begin
        print_endline "This orientation doesn't work with your coordinates.";
        get_orientation col_input row_input length_ship
      end
  | _ ->
      print_endline
        "Invalid orientation. Please enter 'up', 'down', 'left', or 'right'.";
      get_orientation col_input row_input length_ship

(** [get_coords] prompts user for row and column value. Checks if coordinate is
    valid. If invalid, reprompts the user for a new coordiante. Otherwise,adds
    coord to a list . *)
let rec get_coords name_ship board =
  let length_ship = if name_ship = 31 then 3 else name_ship in
  let row_input = get_row_coord () in
  let col_input = get_col_coord () in
  let orientation = get_orientation col_input row_input length_ship in

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

let rec user_turn computer_board =
  let computer_ships = get_occupied_coords () in
  print_endline "Your turn!";
  let row_input = get_row_coord () in
  let col_input =
    Char.code (String.get (get_col_coord ()) 0) - Char.code 'A' + 1
  in
  if not (valid_guess_user row_input col_input) then begin
    print_endline "You have already guessed this spot. Try again.";
    user_turn computer_board
  end
  else begin
    add_user_guess (row_input, col_input);
    if not (check_in_comp_shi_coords row_input col_input) then begin
      print_endline "You missed";
      mark_on_board computer_board (row_input, col_input) "O"
    end
    else begin
      print_endline "Hit!";
      let ship = get_board_element computer_board row_input col_input in
      let ship_name = find_ship_name ship in
      mark_on_board computer_board (row_input, col_input) "X";
      (* Mark a hit *)
      let ship = find_ship_in_list computer_ships ship_name in
      update_ship_hit computer_ships ship_name;
      (* Update the hits on the ship *)
      if is_sunk ship then
        print_endline
          ("You sank the computers ship of length " ^ get_length ship ^ "!")
    end
  end

(** Starts game and asks for name *)
let () =
  welcome ();
  print_endline "Computer Board";
  print_endline string_comp_ships;
  print_endline string_occ_coord;
  print_endline string_row_col;
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
  print_grid (board_array user_board);
  print_newline ();
  user_turn computer_board;
  print_endline "Thanks for playing. Goodbye!"
