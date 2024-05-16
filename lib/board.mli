type board = string array array
(** [board]: A type representing a game board as a 2D array of strings. *)

type coord_list = (int * int) list
(** [coord_list]: A type representing a list of coordinate tuples (int * int). *)

val column_labels : char array
(** [column_labels]: Array of characters used as column labels on the board. *)

val row_labels : string array
(**[row_labels]: Array of strings used as row labels on the board. *)

val user_ship_coords : coord_list ref
(** [user_ship_coords]: Reference to a list storing user's ship coordinates. *)

val user_board_array : board -> string array array
(** [user_board_array]: Function to retrieve the current state of the user's
    board. *)

val in_user_ship_coords : int * int -> bool

(** [in_user_ship_coords]: Checks if a given pair of coordinates is in the
    user's ship coordinates. *)

val create_board : unit -> board
(** [create_board]: Creates a new game board initialized with a board of size 10
    x 10. *)

val get_user_board_element : board -> int -> int -> string
(** [get_user_board_element]: Retrieves the element from the user's board at the
    specified row and column. *)

val get_board_element : board -> int -> int -> string
(** [get_board_element]: Retrieves an element from any given board at the
    specified row and column. *)

val get_first_element : coord_list -> int -> int
(** [get_first_element]: Retrieves the first element of the specified index from
    a list of coordinates. *)

val get_second_element : coord_list -> int -> int
(** [get_second_element]: Retrieves the second element of the specified index
    from a list of coordinates. *)

val add_coords : coord_list -> unit
(** [add_coords]: Adds a list of coordinates to the existing list of user's ship
    coordinates. *)

val valid_guess : int -> int -> bool
(** [valid_guess]: Checks if a guess at the specified row and column is valid
    (e.g., within bounds and not previously guessed). *)

val match_ship : int -> string
(** [match_ship]: Returns the type of ship matched with the given integer
    identifier. *)

val set_board : board -> coord_list -> int -> int -> unit
(** [set_board]: Places ships on the board at the specified coordinates,
    modifying the board state. *)

val check_orientation : (int -> int -> int) -> int -> int -> bool
(** [check_orientation]: Checks if placing a ship with a specific orientation is
    valid from the starting coordinate. *)

val check_ships_coord : board -> coord_list -> int -> int -> bool
(** [check_ships_coord]: Checks if any ships exist at the given coordinates on
    the specified board. *)

val generate_coords :
  (int * int) list -> int -> string -> int -> int -> (int * int) list
(** [generate_coords]: Generates a list of coordinates for placing a ship based
    on starting coordinates, direction, and length. *)

val create_coord_array : string -> int -> int -> int -> int -> board -> bool
(** [create_coord_array]: Validates and creates an array of coordinates based on
    provided specifications and updates the board if necessary. *)
