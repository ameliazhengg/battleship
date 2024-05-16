(* Ships.mli - Interface for the Ships module *)

type board = string array array

(* Initializes a new game board for the computer *)
val create_computer_board : unit -> board

(* Gets the element at the specified row and column of the board *)
val get_comp_board_element : board -> int -> int -> string

(* Generates a random board with ships placed *)
val random_board : board -> board

(* Returns the size of the computer's ship list *)
val get_comp_lst_size : unit -> int

(* Returns a string representation of the computer's ships *)
val string_comp_ships : string

(* Returns a string representation of the occupied coordinates *)
val string_occ_coord : string

(* Returns a string representation of the ship coordinates as row and column *)
val string_row_col : string

(* Generates a list of occupied coordinates from a list of integers *)
val get_occupied_coords : unit -> (int * int) list

(* Checks if a guessed coordinate is occupied by a ship *)
val in_comp_shi_coords : int -> int -> bool

(* Generates a random guess for a row and column *)
val generate_random_guess : string -> int * int
