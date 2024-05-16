type ship = {
  name : int;
  length : int;
  coordinates : (int * int) list;
  hits : int;
}
(** [ship] Represents a ship with properties such as name, length, coordinates,
    and hits *)

val user_ships : ship list ref
(** [user_ships] is a mutable list reference storing the user ships *)

val computer_ships : ship list ref
(** [computer_ships] is a mutable list reference storing the computer ships *)

val get_length : ship -> string
(** [get_length] retrieves the string representation of a ship's length *)

val add_computer_ship : int -> int -> (int * int) list -> unit
(** [add_computer_ship] adds a new ship to the computer's list of ships and
    initializes hits to 0 *)

val add_user_ship : int -> int -> (int * int) list -> unit
(** [add_user_ship] adds a new ship to the user's list of ships. Initializes
    hits to 0 *)

val is_sunk : ship -> bool
(** [is_sunk] checks if a ship is sunk based on its hit count and length.
    Returns true if hits are equal to or greater than length, indicating the
    ship is sunk *)

val check_all_hit : ship list ref -> bool
(** [check_all_hit] checks if the total hits on all ships have reached a certain
    threshold (16) indicating game end. Returns true if all required hits are
    achieved *)

val update_ship_hit : ship list ref -> int -> unit
(**[update_ship_hit] updates the hit count of a specific ship in the given list
   by incrementing its hit count by 1 *)

val find_ship_name : string -> int
(** [find_ship_name] maps ship representation characters to their corresponding
    numerical identifiers. For example, 'a' to 2, 'b' to 3, etc. *)

val find_ship_in_list : ship list -> int -> ship
(** [find_ship_in_list] finds a ship in the provided list by its name identifier *)

val get_comp_ships : unit -> ship list ref
(** [get_comp_ships] retrieves the reference to the list of computer ships *)

val get_user_ships : unit -> ship list ref
(** [get_user_ships] retrieves the reference to the list of user ships *)

val ship_to_string : ship -> string
(** [ship_to_string] returns a string representation of a ship *)

val get_ship_update : string -> ship list ref -> ship
(** [get_ship_update] processes a hit on a ship, updates the hit count, and
    returns the ship *)

val get_comp_hits : unit -> int
(** [get_comp_hits] calculates the total hits on all computer ships *)
