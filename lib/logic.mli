val add_user_guess : int * int -> unit
(** [add_user_guess row col] adds the user's recent guess to the list of
    guesses. *)

val add_computer_guess : int * int -> unit
(** [add_computer_guess guess] adds the computer's recent guess to the list of
    guesses. *)

val valid_guess_user : int -> int -> bool
(** [valid_guess_user row col] checks if the user has already guessed the
    specified coordinates. Returns [true] if the coordinates have not been
    guessed; [false] otherwise. *)

val valid_guess_computer : int * int -> bool
(** [valid_guess_computer guess] checks if the computer has already guessed the
    specified coordinates. Returns [true] if the coordinates have not been
    guessed; [false] otherwise. *)

val mark_on_board : string array array -> int * int -> string -> unit
(** [mark_on_board board (row, col) symbol] marks the board at the specified
    position with the given symbol. *)

val is_valid_row_input : string -> bool
(** [is_valid_row_input input] checks if the input is a valid row number between
    1 and 10. Returns [true] if valid; [false] otherwise. *)

val is_valid_col_input : string -> bool
(** [is_valid_col_input input] checks if the input is a valid column label
    between 'A' and 'J' (case insensitive). Returns [true] if valid; [false]
    otherwise. *)
