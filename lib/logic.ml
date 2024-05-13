let user_guesses : (int * int) list ref = ref []
let computer_guesses : (int * int) list ref = ref []
let add_user_guess (row, col) = user_guesses := (row, col) :: !user_guesses

let add_computer_guess (row, col) =
  computer_guesses := (row, col) :: !computer_guesses

(* check if user has already guessed this coord before*)
let valid_guess_user row col = List.mem (row, col) !user_guesses

(* check if computer has already guessed this coord before*)
let valid_guess_computer row col = List.mem (row, col) !computer_guesses
let mark_on_board board (row, col) symbol = board.(row).(col) <- symbol

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
