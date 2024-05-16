(* Board.mli - Interface for Board module *)

(* Type for the game board, represented as a 2D array of strings *)
type board = string array array

(* Type for a list of coordinates, each represented as a pair of integers *)
type coord_list = (int * int) list

(* Array of column labels, typically characters like 'A', 'B', 'C', etc. *)
val column_labels : char array

(* Array of row labels, typically strings like "1", "2", "3", etc. *)
val row_labels : string array

(* Reference to the list of user ship coordinates *)
val user_ship_coords : coord_list ref

(* Get the array representation of the user's board from the given board *)
val user_board_array : board -> string array array

(* Check if a coordinate is in the list of user ship coordinates *)
val in_user_ship_coords : int * int -> bool

(* Create a new empty board *)
val create_board : int -> board

(* Get the element at the specified row and column from the user's board *)
val get_user_board_element : board -> int -> int -> string

(* Get the element at the specified row and column from the given board *)
val get_board_element : board -> int -> int -> string

(* Get the first element (row) of the nth coordinate in the list *)
val get_first_element : coord_list -> int -> int

(* Get the second element (column) of the nth coordinate in the list *)
val get_second_element : coord_list -> int -> int

(* Add a coordinate to the list of user ship coordinates *)
val add_coords : coord_list -> unit

(* Check if the given coordinates are a valid guess on the board *)
val valid_guess : int -> int -> bool

(* Match the given integer to a corresponding ship type represented as a
   string *)
val match_ship : int -> string

(* Set the board at the given coordinates with the specified value *)
val set_board : board -> coord_list -> int -> int -> unit

(* Check the orientation of the ship using a provided function *)
val check_orientation : (int -> int -> int) -> int -> int -> bool

(* Check if the ship coordinates are valid on the given board *)
val check_ships_coord : board -> coord_list -> int -> int -> bool

(* Generate a list of coordinates for a ship given the starting position,
   orientation, and length *)
val generate_coords :
  (int * int) list -> int -> string -> int -> int -> (int * int) list

(* Create an array of coordinates for placing a ship on the board *)
val create_coord_array : string -> int -> int -> int -> int -> board -> bool
