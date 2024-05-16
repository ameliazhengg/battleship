(* list representing the guesses the user has made*)
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

(** [is_valid_row_input] requires [input] to be None or Some. Returns: true if
    input is an integer within 1-10, and false otherwise. *)

let is_valid_row_input input =
  let row_num = int_of_string input in
  if row_num > 1 && row_num <= 10 then true else false

(** [is_valid_col_input] requires [input] to be None or Some. Returns: true if
    input is a string between A-J case insensitive, and false otherwise. *)
let is_valid_col_input input =
  match String.length input with
  | 1 ->
      let char_code = Char.code (String.get input 0) in
      (char_code >= Char.code 'A' && char_code <= Char.code 'J')
      || (char_code >= Char.code 'a' && char_code <= Char.code 'j')
  | _ -> false
