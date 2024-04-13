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
