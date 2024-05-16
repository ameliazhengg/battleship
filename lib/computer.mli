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

val check_contains : int -> int -> int -> int list -> bool
(** [check_contains] is true if the coordinates of a potential ship are already
    occupied otherwise false *)

val valid_placement : int -> int -> int -> int list -> bool
(** [valid_placement] is true if a ship with [start_coord] [dir] and [len] can
    be placed there otherwise false *)

val random_coord : 'a -> int
(** [random_coord] is a random number between 1 and 100 inclusive representing
    the coordinate in a 10x10 grid *)

val comp_ship_coords : (int * int) list ref
val occupied_coords : int list ref

val add_ship_to_lst : int -> int -> int -> int -> (int * int) list -> unit
(** [add_ship_to_lst] takes a new ship and adds it to [occupied_coords] and
    [comp_ship_coords] and creates a new [ship] *)

val new_ship_coord : int -> int -> int -> int -> unit
val add_coords : unit -> unit
val ship_match : int -> string
