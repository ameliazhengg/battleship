type board
type coord

(*let computer_ship_coords = ref []*)
val create_board : unit -> board
val get_board_element : board -> int -> int -> string
val random_dir : unit -> int

(* [check_contains] is true if the coordinates of a potential ship are already*)
val check_contains : int -> int -> int -> int list -> bool

(* [valid_placement] is true if a ship with [start_coord] [dir] and [len] can be
   placed there otherwise false *)
val valid_placement : int -> int -> int -> int list -> bool

(* [random_coord] is a random number between 1 and 100 inclusive representing
   the coordinate in a 10x10 grid *)
val random_coord : 'a -> int

(* [add_ship_to_lst] takes a new ship and adds it to [occupied_coords] and
   [comp_ship_coords] and creates a new [ship] *)
(* val add_ship_to_lst : int -> int -> int -> int -> list -> unit *)
