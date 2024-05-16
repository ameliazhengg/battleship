type board = string array array
type coord_list = (int * int) list

val column_labels : char array
val row_labels : string array
val user_ship_coords : coord_list ref
val user_board_array : board -> string array array
val in_user_ship_coords : int * int -> bool
val create_board : unit -> board
val get_user_board_element : board -> int -> int -> string
val get_board_element : board -> int -> int -> string
val get_first_element : coord_list -> int -> int
val get_second_element : coord_list -> int -> int
val add_coords : coord_list -> unit
val valid_guess : int -> int -> bool
val match_ship : int -> string
val set_board : board -> coord_list -> int -> int -> unit
val check_orientation : (int -> int -> int) -> int -> int -> bool
val check_ships_coord : board -> coord_list -> int -> int -> bool

val generate_coords :
  (int * int) list -> int -> string -> int -> int -> (int * int) list

val create_coord_array : string -> int -> int -> int -> int -> board -> bool
