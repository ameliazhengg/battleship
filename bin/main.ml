open Final_proj_3110.Board
open Final_proj_3110.Logic
open Final_proj_3110.Computer
open Final_proj_3110.Ships

(* ADDING COLORS *)
let reset_color = "\027[0m"
let move_color = "\x1b[38;5;196m"
let user_theme = ref 0

(*[themes_list] is the set of all themes *)
let themes_list =
  [
    [
      "\x1b[38;5;215m";
      "\x1b[38;5;216m";
      "\x1b[38;5;217m";
      "\x1b[38;5218m";
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

(* [get_theme] prompts user for theme *)
let get_theme = List.nth themes_list !user_theme

(* [get_color] returns color based on letter *)
let get_color elt =
  match elt with
  | " a " -> List.nth get_theme 0
  | " b " -> List.nth get_theme 1
  | " c " -> List.nth get_theme 2
  | " d " -> List.nth get_theme 3
  | " e " -> List.nth get_theme 4
  | _ -> ""

(*[print_battle] prints battle in ascii art *)
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

(*[print_of] prints of in ascii art *)
let print_of () =
  let banner =
    [
      "  ______    _______ ";
      " /  __  \\  |   ____|    ₊✧⋆⭒˚｡₊✧⋆⭒˚｡⋆";
      "|  |  |  | |  |__    ₊✧⋆⭒˚｡⋆      ₊✧⋆⭒˚｡⋆";
      "|  |  |  | |   __|       ₊✧⋆⭒˚｡⋆ ₊✧⋆⭒˚｡⋆";
      "|  `--'  | |  |   ₊✧⋆⭒˚｡⋆  ₊✧⋆⭒˚｡⋆";
      " \\______/  |__|     ";
      "                    ";
    ]
  in
  List.iter print_endline banner

(* [print_ships] prints ships in ascii *)
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

(*[pick_theme] propmts user to pick them *)
let rec pick_theme () =
  print_newline ();
  print_endline "List of themes : ";
  print_endline "Theme 1 : Peach Vibe";
  print_endline "Theme 2 : Tropical Vibe";
  print_endline "Theme 3 : Arctic Vibe";
  print_endline "Theme 4 : Oasis Vibe";
  print_endline "Theme 5 : Cosmic Vibe";
  print_endline "Theme 6 : Fantasy Vibe";
  print_string "Enter a theme number : ";
  let input = read_line () in
  try
    let input_int = int_of_string input in
    if input_int > 0 && input_int < 7 then (
      print_endline ("You picked theme " ^ input ^ "!\n");
      user_theme := input_int)
    else (
      print_endline "Invalid input. Try again!\n";
      pick_theme ())
  with Failure _ ->
    print_endline "Invalid input. Try again!\n";
    pick_theme ()

(*[instructions] is the instructions on how to play the game *)
let instructions () =
  print_endline
    "You have five ships consisting of lengths 2,3,3,4 and 5. These will be \
     represented on your board as 'a', 'b', 'c', 'd', and 'e', respectively. \
     The battleship board is 10x10, with the columns being labeled A-J and the \
     rows 1-10.";
  print_newline ();

  print_endline
    "Directions: To initiate your board, you will be prompted to enter a \
     coordinate for your ship and an orientation. You can must first enter the \
     row number, and then the column letter, case insensitive. You can place \
     your ships horizontally or vertically, but not diagonally. You cannot \
     place your ships in a manner that overlaps with any other ships or beyond \
     the grid. You and the computer will alternate turns guesssing. If you \
     have hit one of the computer's ships, the terminal will display the board \
     and hit square will be shown with an 'X'. If you have missed, it will be \
     shown as an 'O'. The first player to hit all 5 shapes wins the game.";
  print_newline ();

  print_endline
    "Themes: To utilize the available color theme presets, select the desired \
     palette when prompted by the terminal to do so. The available themes are \
     named Peach Vibe, Tropical Vibe, Arctic Vibe, Oasis Vibe, Cosmic Vibe, \
     and Fantasy Vibe. Running the main.exe file will automatically prompt the \
     user to choose a theme.";
  print_newline ();

  print_endline
    "Modes: Our game enables three different modes. Easy, medium or hard. You \
     will be prompted to chose one of these before the game starts. Easy mode \
     is when the computer's guesses are completely randomized. In Medium mode \
     the computer has a more strategic aproach, guessing every other square. \
     In Hard mode, the computer takes into account if it has hit a ship and \
     takes the surrounding spots of the hit ship into consideration.";
  print_newline ();

  print_endline
    "Cheat mode: Our game enables cheat mode and non-cheat mode. You will be \
     prompted to choose either to enter cheat mode or non-cheat mode. Enabling \
     cheat mode allows you to view the computers board and all of the \
     computer's ships. This way, you can cheat and win easily! If you decide \
     to take the hard way out, then you select N when prompted whether to \
     enter cheat mode or not. This will conceal all of the computer's ships. "

(*[welcome] is the welcome message *)
let welcome () =
  print_newline ();
  print_string "Please enter your name: ";
  let player_name = read_line () in
  print_endline ("Welcome to Battleship, " ^ player_name ^ "!");

  let rec ask_instructions () =
    print_string
      "\nWould you like to read how to play battleship? (yes or no): ";
    let input = String.uppercase_ascii (read_line ()) in
    match input with
    | "YES" -> instructions ()
    | "NO" -> print_endline "The game begins now."
    | _ ->
        print_endline "Invalid input. Please type 'yes' or 'no'.";
        ask_instructions ()
  in
  ask_instructions ()

(** prints row with the battleships *)
let print_row board i =
  print_string "  ";
  for x = 0 to 9 do
    print_string "|";
    let elt = get_board_element board i x in
    print_string (get_color elt ^ elt ^ reset_color)
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

(* gets the row coordinate that user gueses *)
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
  let col_input = String.uppercase_ascii (read_line ()) in
  if is_valid_col_input col_input then col_input
  else begin
    print_endline "Invalid column input. Please enter a valid column letter.";
    get_col_coord length_ship
  end

(* guess column coord that user guesses *)
let rec get_guess_col_coord () =
  print_string "Enter the column letter for your guess (A-J): ";
  let col_input = String.uppercase_ascii (read_line ()) in
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
  let col_input = get_col_coord length_ship in
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

(* actions called if user misses *)
let if_user_missed computer_board row_input col_input conceal =
  begin
    print_endline "You missed";
    mark_on_board computer_board (row_input, col_input)
      (move_color ^ " O " ^ reset_color);
    add_user_incorrect_guess (row_input, col_input);
    print_endline "Computer Board";
    if conceal = "y" then begin
      print_grid computer_board
    end
    else print_grid (populate_concealed_board (create_concealed_board ()))
  end

(* actions called if user hits *)
let if_user_hit computer_board row_input col_input =
  begin
    print_endline "Hit!";
    let rec_coords = get_rec_coords_user row_input col_input in
    if rec_coords = [] then
      print_endline "There are no recommended next guesses"
    else
      print_endline
        ("Recommended next guesses: " ^ string_of_list_coords rec_coords);
    let ship_rep =
      get_comp_board_element computer_board (row_input - 1) (col_input - 1)
    in
    let ship = get_ship_update ship_rep computer_ships in
    mark_on_board computer_board (row_input, col_input)
      (move_color ^ " X " ^ reset_color);
    add_user_correct_guess (row_input, col_input);
    ship
  end

(* users turn. allows user to choose a column and row and tells user if their
   guess is correct *)
let rec user_turn computer_board user_board mode conceal =
  let computer_ships = get_comp_ships () in
  print_endline "Your turn!";
  let row_input = get_guess_row_coord () in
  let col_input =
    Char.code (String.get (get_guess_col_coord ()) 0) - Char.code 'A' + 1
  in
  if not (valid_guess_user row_input col_input) then begin
    print_endline "You have already guessed this spot. Try again.";
    user_turn computer_board user_board mode conceal
  end
  else begin
    add_user_guess (row_input, col_input);
    if not (in_comp_shi_coords row_input col_input) then begin
      if_user_missed computer_board row_input col_input conceal;
      computer_turn user_board computer_board mode conceal
    end
    else begin
      let ship = if_user_hit computer_board row_input col_input in
      print_newline ();
      (* Update the hits on the ship *)
      if is_sunk ship then begin
        print_endline
          ("You sank the computers ship of length " ^ get_length ship ^ "!");
        if check_all_hit computer_ships then print_endline "Congrats, you won!"
        else begin
          if conceal = "y" then print_grid computer_board
          else print_grid (populate_concealed_board (create_concealed_board ()));
          computer_turn user_board computer_board mode conceal
        end
      end
      else begin
        print_endline "Computer Board";
        if conceal = "y" then print_grid computer_board
        else print_grid (populate_concealed_board (create_concealed_board ()));
        computer_turn user_board computer_board mode conceal
      end
    end
  end

(* prompts computer to guess randomly based on selected difficulty *)
and computer_turn user_board computer_board mode conceal =
  let user_ships = get_user_ships () in
  print_endline "Now the computer will take a guess!";
  Unix.sleepf 1.;
  let guess = generate_random_guess mode in
  if valid_guess_computer guess then begin
    add_computer_guess guess;
    let row, col = guess in
    let row_str = string_of_int row in
    let col_str = Char.escaped (char_of_int (col + int_of_char 'A' - 1)) in
    if not (in_user_ship_coords guess) then begin
      Printf.printf "The computer guessed %s%s and missed :(\n" row_str col_str;
      mark_on_board (user_board_array user_board) guess " O ";
      (* Mark miss on the board *)
      print_newline ();
      print_endline "Your Board";
      print_grid user_board;
      user_turn computer_board user_board mode
        conceal (* Print the updated board *)
    end
    else begin
      Printf.printf "The computer guessed %s%s and hit!\n" row_str col_str;
      add_recommended guess;
      (* Mark hit on the board *)
      let ship_rep = get_user_board_element user_board row col in
      mark_on_board (user_board_array user_board) guess " X ";

      let ship = get_ship_update ship_rep user_ships in
      if is_sunk ship then begin
        print_endline
          ("The computer has sank your ship of length " ^ get_length ship ^ "!");
        if check_all_hit user_ships then begin
          print_endline "Aw no, the computer won!";
          print_grid user_board
        end
        else begin
          print_newline ();
          print_endline "Your Board";
          print_grid user_board;
          user_turn computer_board user_board mode conceal
        end
      end
      else begin
        print_newline ();
        print_endline "Your Board";
        print_grid user_board;
        user_turn computer_board user_board mode conceal
      end
    end
  end
  else computer_turn user_board computer_board mode conceal
(* Retry the turn if the guess was not valid *)

(* Start the game with initial calls to user_turn and computer_turn as needed *)
let pick_mode () =
  print_string "What mode do you want to play? (easy, medium, hard): ";
  let rec get_valid_mode () =
    let input = read_line () |> String.trim |> String.lowercase_ascii in
    match input with
    | "easy" | "medium" | "hard" -> input
    | _ ->
        print_string
          "Invalid mode selected. Please choose from easy, medium, or hard: ";
        get_valid_mode ()
  in
  get_valid_mode ()

(* propmts user to indicate whether or not they want to cheat *)
let pick_conceal () =
  print_string "Do you want to cheat? (Y/N): ";
  let rec get_valid_conceal () =
    let input = read_line () |> String.trim |> String.lowercase_ascii in
    match input with
    | "y" | "n" -> input
    | _ ->
        print_string "Invalid response. Please choose between Y and N: ";
        get_valid_conceal ()
  in
  get_valid_conceal ()

(** Starts game and asks for name *)
let () =
  print_battle ();
  print_of ();
  print_ships ();
  welcome ();
  pick_theme ();
  let mode = pick_mode () in
  let conceal = pick_conceal () in
  print_newline ();
  print_endline "Computer Board";
  let computer_board = create_computer_board () in
  print_grid (random_board computer_board);
  let user_board = create_board () in
  print_endline "";
  print_endline "Now please choose the coordinates of your ships ";
  print_newline ();
  get_coords 2 user_board;
  get_coords 3 user_board;
  get_coords 31 user_board;
  get_coords 4 user_board;
  get_coords 5 user_board;
  print_newline ();
  print_endline "Your Battleship Board";
  print_grid (user_board_array user_board);
  print_newline ();
  user_turn computer_board user_board mode conceal
