val user_guesses : (int * int) list ref
(** [user_guesses]: A list of the coordinates of the user's guesses *)

val computer_guesses : (int * int) list ref
(** [computer_guesses]: A list of the coordinates of the computer's guesses *)

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

val correct_user_guess : (int * int) list ref
(** [correct_user_guess] is the list of the guesses that the users guessed
    correctly *)

val add_user_correct_guess : int * int -> unit
(** [add_user_correct_guess] adds a new guess to the list of
    [correct_user_guess]*)

val get_correct_user_guess : unit -> (int * int) list
val incorrect_user_guess : (int * int) list ref
val add_user_incorrect_guess : int * int -> unit
val get_incorrect_user_guess : unit -> (int * int) list

val mark_on_board : string array array -> int * int -> string -> unit
(** [mark_on_board board (row, col) symbol] marks the board at the specified
    position with the given symbol. *)

val string_of_coord : int * char -> string
(** [string_of_coord] Converts a coordinate pair from (int, int) to a formatted
    string "row,column" where column is a char. *)

val convert_coords_to_game_format : (int * int) list -> (int * char) list
(** [convert_coords_game_format] Converts the integer representation of column
    to character representation *)

val string_of_list_coords : (int * int) list -> string
(** [string_of_list_coords] Converts a list of coordinates from (int, int)
    format to (int, char) format and returns the formatted string of these
    coordinates. *)

val is_on_board_edge : int -> int -> bool
(** [is_on_board_edge] Checks if a given row and column coordinate is on the
    edge of a 10x10 game board. *)

val filter_valid_coords : (int * int) list -> (int * int) list
(** [filter_valid_coords]Filters a list of coordinates, removing those that have
    been guessed by the computer. Assumes a function valid_guess_computer is
    defined elsewhere. *)

val get_edge_coords : int -> int -> (int * int) list
(** [get_edge_coords] Gets the surrounding edge coordinates for a given
    coordinate on the board, excluding out-of-bounds coordinates. *)

val get_corner_coords : int -> int -> (int * int) list
(** [get_corner_coords]Determines the specific corner-related coordinates based
    on the board position. Handles special cases for corners of a 10x10 grid. *)

val get_rec_coords_user : int -> int -> (int * int) list
(** [get_rec_coords_user]Computes recommended coordinates for a given position
    on a 10x10 grid considering whether the position is on the edge or a corner. *)

val rec_guesses : (int * int) list ref
val get_rec_1 : unit -> int * int
val add_recommended : int * int -> unit
val remove_recommended : unit -> unit
val size_of_recommended : unit -> int

val is_valid_row_input : string -> bool
(** [is_valid_row_input input] checks if the input is a valid row number between
    1 and 10. Returns [true] if valid; [false] otherwise. *)

val is_valid_col_input : string -> bool
(** [is_valid_col_input input] checks if the input is a valid column label
    between 'A' and 'J' (case insensitive). Returns [true] if valid; [false]
    otherwise. *)
