open Final_proj_3110.Board
open Final_proj_3110.Logic
open Final_proj_3110.Computer
open Final_proj_3110.Ships

(* ADDING COLORS *)
let reset_color = "\027[0m"
let hit_color = "\x1b[38;5;196m"

let print_battle () =
  let banner =
    [
      ".______        ___   .___________.___________. __       _______ ";
      "|   _  \\      /   \\  |           |           ||  |     |   ____|";
      "|  |_)  |    /  ^  \\ `---|  |----`---|  |----`|  |     |  |__   ";
      "|   _  <    /  /_\\  \\    |  |        |  |     |  |     |   __|  ";
      "|  |_)  |  /  _____  \\   |  |        |  |     |  `----.|  |____ ";
      "|______/  /__/     \\__\\  |__|        |__|     |_______||_______|";
    ]
  in
  List.iter print_endline banner

let print_of () =
  let banner =
    [
      "  ______    _______ ";
      " /  __  \\  |   ____|";
      "|  |  |  | |  |__   ";
      "|  |  |  | |   __|  ";
      "|  `--'  | |  |     ";
      " \\______/  |__|     ";
      "                    ";
    ]
  in
  List.iter print_endline banner

let print_ships () =
  let banner =
    [
      "     _______. __    __   __  .______     _______.";
      "    /       ||  |  |  | |  | |   _  \\   /       |";
      "   |   (----`|  |__|  | |  | |  |_)  | |   (----`";
      "    \\   \\    |   __   | |  | |   ___/   \\   \\    ";
      ".----)   |   |  |  |  | |  | |  |   .----)   |   ";
      "|_______/    |__|  |__| |__| | _|   |_______/    ";
      "                                                 ";
    ]
  in
  List.iter print_endline banner

let pick_theme () =
  print_newline ();
  print_endline "List of themes : ";
  print_endline "Theme 1 : Peach Vibe";
  print_endline "Theme 2 : Tropical Vibe";
  print_endline "Theme 3 : Arctic Vibe";
  print_endline "Theme 4 : Oasis Vibe";
  print_endline "Theme 5 : Cosmic Vibe";
  print_endline "Theme 6 : Fantasy Vibe";
  print_string "Enter a theme number : ";
  let user_theme = read_line () in
  print_endline ("You picked " ^ user_theme ^ "!\n");
  user_theme

let instructions () =
  print_endline "How to play battleship:";
  print_endline
    "You have five ships consisting of lengths 2,3,3,4 and 5. These will be \
     represented on your board as 'a', 'b', 'c', 'd', and 'e', respectively";
  print_endline
    "To initiate your board, you will be prompted to enter a coordinate for \
     your ship and an orientation."
(* FINISH LATER*)

let welcome () =
  print_newline ();
  print_string "Please enter your name: ";
  let player_name = read_line () in
  print_endline ("Welcome to Battleship, " ^ player_name ^ "!");
  print_string "\nWould you like to read how to play battleship? (yes or no): ";
  let input = read_line () in
  if String.uppercase_ascii input = "YES" then instructions ()
  else print_endline "The game begins now."

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

let rec get_guess_row_coord () =
  print_string "Enter the row number for your guess (1-10): ";
  let row_input = read_line () in
  if is_valid_row_input row_input then int_of_string row_input
  else begin
    print_endline "Invalid row input. Please enter a valid row number.";
    get_guess_row_coord ()
  end

(* gets the users input for row *)
let rec get_row_coord length_ship =
  print_string
    ("Enter the row number for your " ^ string_of_int length_ship
   ^ "-length ship (1-10): ");
  let row_input = read_line () in
  if is_valid_row_input row_input then int_of_string row_input
  else begin
    print_endline "Invalid row input. Please enter a valid row number.";
    get_row_coord length_ship
  end

(*gets user input for col*)
let rec get_col_coord length_ship =
  print_string
    ("Enter the column letter for your " ^ string_of_int length_ship
   ^ "-length ship (A-J): ");
  let col_input = read_line () in
  if is_valid_col_input col_input then col_input
  else begin
    print_endline "Invalid column input. Please enter a valid column letter.";
    get_col_coord length_ship
  end

let rec get_guess_col_coord () =
  print_string "Enter the column letter for your guess (A-J): ";
  let col_input = read_line () in
  if is_valid_col_input col_input then col_input
  else begin
    print_endline "Invalid column input. Please enter a valid column letter.";
    get_guess_col_coord ()
  end

(*gets user input for orientation*)
let rec get_orientation col_input row_input length_ship =
  print_string "Orientation (up, down, left, right): ";
  let user_input = read_line () in
  match user_input with
  | "left" ->
      if check_orientation ( - ) (int_of_char col_input.[0] - 64) length_ship
      then "left"
      else begin
        print_endline "This orientation doesn't work with your coordinates.";
        get_orientation col_input row_input length_ship
      end
  | "right" ->
      if check_orientation ( + ) (int_of_char col_input.[0] - 64) length_ship
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
  let row_input = get_row_coord length_ship in
  let col_input = String.uppercase_ascii (get_col_coord length_ship) in
  let orientation = get_orientation col_input row_input length_ship in

  match
    create_coord_array orientation row_input
      (int_of_char col_input.[0] - 64)
      name_ship length_ship board
    (* nums is the name of the ship 1,2,3,31,4,5 and n is just the length of
       ship*)
  with
  | true -> ()
  | false ->
      print_endline "Cannot add coordinates due to overlap";
      get_coords name_ship board

let rec user_turn computer_board user_board =
  let computer_ships = get_comp_ships () in
  print_endline "Your turn!";
  let row_input = get_guess_row_coord () in
  let col_input =
    Char.code (String.get (get_guess_col_coord ()) 0) - Char.code 'A' + 1
  in
  if not (valid_guess_user row_input col_input) then begin
    print_endline "You have already guessed this spot. Try again.";
    user_turn computer_board user_board
  end
  else begin
    add_user_guess (row_input, col_input);
    if not (in_comp_shi_coords row_input col_input) then begin
      print_endline "You missed";
      mark_on_board computer_board (row_input, col_input) " O ";
      print_grid computer_board;
      computer_turn user_board computer_board
    end
    else begin
      print_endline "Hit!";
      print_endline (string_of_int (get_comp_hits ()));
      mark_on_board computer_board (row_input, col_input)
        (hit_color ^ " X " ^ reset_color);
      let ship_rep =
        get_comp_board_element computer_board (row_input - 1) (col_input - 1)
      in
      let ship = get_ship_update ship_rep computer_ships in
      print_endline (ship_to_string ship);
      (* Update the hits on the ship *)
      if is_sunk ship then begin
        print_grid computer_board;
        print_endline "is_sunk ran";
        print_endline
          ("You sank the computers ship of length " ^ get_length ship ^ "!");
        if check_all_hit computer_ships then print_endline "Congrats, you won!"
        else begin
          print_grid computer_board;
          computer_turn user_board computer_board
        end
      end
      else begin
        print_grid computer_board;
        computer_turn user_board computer_board
      end
    end
  end

and computer_turn user_board computer_board =
  let user_ships = get_user_ships () in
  print_endline "Now the computer will take\n   a guess!";
  Unix.sleepf 1.;
  let guess = generate_random_guess () in
  if valid_guess_computer guess then begin
    add_computer_guess guess;
    let row, col = guess in
    let row_str = string_of_int row in
    let col_str = Char.escaped (char_of_int (col + int_of_char 'A' - 1)) in
    if not (in_user_ship_coords guess) then begin
      Printf.printf "The computer guessed %s%s and missed :(\n" row_str col_str;
      mark_on_board (user_board_array user_board) guess " O ";
      (* Mark miss on the board *)
      print_grid user_board;
      user_turn computer_board user_board (* Print the updated board *)
    end
    else begin
      Printf.printf "The computer guessed %s%s and hit!\n" row_str col_str;
      mark_on_board (user_board_array user_board) guess " X ";
      (* Mark hit on the board *)
      let ship_rep = get_user_board_element user_board row col in
      let ship = get_ship_update ship_rep user_ships in
      if is_sunk ship then begin
        print_endline
          ("The computer has sank your ship of length " ^ get_length ship ^ "!");
        if check_all_hit user_ships then begin
          print_endline "Aw no, the computer won!";
          print_grid user_board
        end
        else begin
          print_grid user_board;
          user_turn computer_board user_board
        end
      end
      else begin
        print_grid user_board;
        user_turn computer_board user_board
      end
    end
  end
  else computer_turn user_board computer_board
(* Retry the turn if the guess was not valid *)

(* Start the game with initial calls to user_turn and computer_turn as needed *)

(** Starts game and asks for name *)
let () =
  print_battle ();
  print_of ();
  print_ships ();
  welcome ();
  let theme = pick_theme () in

  print_endline "Computer Board";
  print_endline string_comp_ships;
  print_endline string_occ_coord;
  print_endline string_row_col;
  let computer_board = create_computer_board () in
  print_grid (random_board computer_board);
  print_endline "Your Battleship Board";
  let user_board = create_board (int_of_string theme) in
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
  print_grid (user_board_array user_board);
  print_newline ();
  user_turn computer_board user_board;
  print_endline "Thanks for playing. Goodbye!"
