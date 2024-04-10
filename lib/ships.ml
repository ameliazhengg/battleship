type ship = {
  name : string;
  length : int;
  coordinates : (int * char) list;
  hits : int;
}

type guesses = (int * char) list

let coord_add coord str str2 = (str, str2) :: coord

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

(* checks if the guess is valid, checks if its valid input and then check if its
   already been guessed*)
let is_valid_guess row col = is_valid_row_input row && is_valid_col_input col
let create_ship name length a = { name; length; coordinates = a; hits = 0 }

(**let output match_emoji = function | true -> "❌" | false -> "⚪️" **)
