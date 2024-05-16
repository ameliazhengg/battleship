type ship = {
  name : int; (* The unique identifier for the ship *)
  length : int; (* The length of the ship *)
  coordinates : (int * int) list; (* The coordinates occupied by the ship *)
  hits : int; (* The number of times the ship has been hit *)
}

(* Reference to the list of user ships *)
val user_ships : ship list ref

(* Reference to the list of computer ships *)
val computer_ships : ship list ref

(* Get the length of a ship as a string *)
val get_length : ship -> string

(* Add a ship to the computer's list of ships with the given name, length, and
   coordinates *)
val add_computer_ship : int -> int -> (int * int) list -> unit

(* Add a ship to the user's list of ships with the given name, length, and
   coordinates *)
val add_user_ship : int -> int -> (int * int) list -> unit

(* Check if a ship is sunk (i.e., the number of hits equals the length of the
   ship) *)
val is_sunk : ship -> bool

(* Check if all ships in the given list are sunk *)
val check_all_hit : ship list ref -> bool

(* Update the number of hits for a ship in the given list of ships by its
   name *)
val update_ship_hit : ship list ref -> int -> unit

(* Find the unique identifier of a ship by its name (assuming ships are named as
   strings) *)
val find_ship_name : string -> int

(* Find a ship in the given list of ships by its name *)
val find_ship_in_list : ship list -> int -> ship

(* Get the reference to the computer's list of ships *)
val get_comp_ships : unit -> ship list ref

(* Get the reference to the user's list of ships *)
val get_user_ships : unit -> ship list ref

(* Convert a ship to its string representation *)
val ship_to_string : ship -> string

(* Get the updated ship from the given list of ships by its name *)
val get_ship_update : string -> ship list ref -> ship

(* Get the total number of hits on all computer ships *)
val get_comp_hits : unit -> int
