let prompt_and_print () =
  let () = print_string "Please enter your name: " in
  let the_input = read_line () in
  print_endline ("Welcome to Battleship, " ^ the_input)

(* Define constants *)
let grid_size = 10
let column_labels = [| "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J" |]
let row_labels = [| "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10" |]

(* Function to print a single cell *)
let print_cell () = print_string "|   "

(* Function to print a row separator *)
let print_separator () =
  print_endline "\n     -----------------------------------------"

(* Function to print a single row *)
let print_row () =
  print_string "  ";
  for _ = 1 to grid_size do
    print_cell ()
  done;
  print_string "|\n";
  print_separator ()

(* Function to print column labels *)
let print_column_labels () =
  print_string "     ";
  Array.iter (fun label -> print_string (label ^ "   ")) column_labels;
  print_endline ""

(* Function to print the entire grid *)
let print_grid () =
  print_column_labels ();
  for i = 1 to grid_size do
    print_string (row_labels.(i - 1) ^ " ");
    print_row ()
  done

(* [is_valid_row_input] requires [input] to be None or Some. Returns: true if
   input is an integer within 1-10, and false otherwise. *)
let is_valid_row_input input =
  let row_num = int_of_string_opt input in
  match row_num with
  | Some num -> num >= 1 && num <= 10
  | None -> false

(* [is_valid_col_input] requires [input] to be None or Some. Returns: true if
   input is a uppercase string between A-J, and false otherwise. *)
let is_valid_col_input input =
  match String.length input with
  | 1 ->
      let char_code = Char.code (String.get input 0) in
      char_code >= Char.code 'A' && char_code <= Char.code 'J'
  | _ -> false

(* [prompt_and_print_grid_id] prompts user for row and column value. Checks if
   coordinate is valid. If invalid, reprompts the user for a new coordiante.
   Otherwise, prints coordinate. *)
let prompt_and_print_grid_id () =
  let rec prompt_for_valid_row_input () =
    let () = print_string "Please enter the row number (1-10): " in
    let row_input = read_line () in
    if is_valid_row_input row_input then row_input
    else begin
      print_endline "Invalid row input. Please enter a valid row number.";
      prompt_for_valid_row_input ()
    end
  in
  let rec prompt_for_valid_col_input () =
    let () = print_string "Please enter the column letter (A-J): " in
    let col_input = read_line () in
    if is_valid_col_input col_input then col_input
    else begin
      print_endline "Invalid column input. Please enter a valid column letter.";
      prompt_for_valid_col_input ()
    end
  in
  let row_input = prompt_for_valid_row_input () in
  let col_input = prompt_for_valid_col_input () in
  print_endline ("You have chosen coordinate: " ^ row_input ^ col_input)

(* Starts game and asks for name *)
let () = prompt_and_print ()

(* Print the grid *)
let () = print_grid ()

(* Asks user for grid row and column and prints out the coordiantes *)
let () = prompt_and_print_grid_id ()

(* Print goodbye message *)
let () = print_endline "Thanks for playing. Goodbye!"
