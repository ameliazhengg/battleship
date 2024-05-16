type board = string array array
(** [board] represents the type of a board*)

val themes_list : string list list
(** [themese_list] is the list of all themes *)

val create_computer_board : unit -> board
(** [create_computer_board] Initializes a new game board for the computer *)

val get_comp_board_element : board -> int -> int -> string
(** [get_comp_board_element] Gets the element at the specified row and column of
    the board *)

val random_board : board -> board
(** [random_board] Generates a random board with ships placed *)

val get_comp_lst_size : unit -> int
(** [get_comp_lst_size] Returns the size of the computer's ship list *)

val string_comp_ships : string
(** [string_comp_ships] Returns a string representation of the computer's ships *)

val string_occ_coord : string
(** [string_occ_coord] Returns a string representation of the occupied
    coordinates *)

val string_row_col : string
(** [string_row_col] Returns a string representation of the ship coordinates as
    row and column *)

val get_occupied_coords : unit -> (int * int) list
(** [get_occupied_coords ] Generates a list of occupied coordinates from a list
    of integers *)

val in_comp_shi_coords : int -> int -> bool
(** [in_comp_shi_coords] Checks if a guessed coordinate is occupied by a ship *)

val generate_random_guess : string -> int * int
(** [generate_random_guess] Generates a random guess for a row and column *)

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
(** [comp_ship_coords] represnets a list of coordinates from the computer*)

val occupied_coords : int list ref
(** [occupied_coords] represents a list of coordinates representing the occupied
    spots on the board *)

val add_ship_to_lst : int -> int -> int -> int -> (int * int) list -> unit
(** [add_ship_to_lst] takes a new ship and adds it to [occupied_coords] and
    [comp_ship_coords] and creates a new [ship] *)

val new_ship_coord : int -> int -> int -> int -> unit
(** [new_ship_coord] is a random new ship starting coordinate that can be placed
    onto the board *)

val add_coords : unit -> unit
(** [add_coords] adds all the coords for each size ship *)

val ship_match : int -> string
(** [ship_match] pattern matches ship name to ship color printed on board *)

val create_concealed_board : unit -> string array array
(** [create_concealed_board] creates a concealed board *)

val populate_concealed_board : board -> board
(** [populate_concealed_board] only shows the coordinates that the user already
    guessed *)

val medium_gen : int -> int
(* [medium_gen] is a randomly generated column number given a certain row
   number *)
