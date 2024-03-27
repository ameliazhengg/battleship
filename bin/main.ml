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

let () = prompt_and_print ()

(* Print the grid *)
let () = print_grid ()
