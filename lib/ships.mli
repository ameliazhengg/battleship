(* Type Definitions *)

type ship = {
  name : int; (* The unique identifier for the ship *)
  length : int; (* The length of the ship *)
  coordinates : (int * int) list; (* The coordinates occupied by the ship *)
  hits : int; (* The number of times the ship has been hit *)
}

(* References to hold the list of user and computer ships *)
val user_ships : ship list ref
val computer_ships : ship list ref

val get_length : ship -> string
(** [get_length ship] returns the length of the ship as a string. *)

val add_computer_ship : int -> int -> (int * int) list -> unit
(** [add_computer_ship name length coords] adds a ship to the computer's list of
    ships with the specified name, length, and coordinates.*)

val add_user_ship : int -> int -> (int * int) list -> unit
(** [add_user_ship name length coords] adds a ship to the user's list of ships
    with the specified name, length, and coordinates.*)

val is_sunk : ship -> bool
(** [is_sunk ship] returns true if the ship is sunk (i.e., the number of hits
    equals the length of the ship), false otherwise. *)

val check_all_hit : ship list ref -> bool
(** [check_all_hit ships] returns true if all ships in the given list are sunk,
    false otherwise.*)

val update_ship_hit : ship list ref -> int -> unit
(** [update_ship_hit ships name] updates the number of hits for a ship in the
    given list of ships by its name. *)

val find_ship_name : string -> int
(** [find_ship_name name] returns the unique identifier of a ship by its name. *)

val find_ship_in_list : ship list -> int -> ship
(** [find_ship_in_list ships name] finds a ship in the given list of ships by
    its name. *)

val get_comp_ships : unit -> ship list ref
(** [get_comp_ships ()] returns the reference to the computer's list of ships. *)

val get_user_ships : unit -> ship list ref
(** [get_user_ships ()] returns the reference to the user's list of ships. *)

val ship_to_string : ship -> string
(** [ship_to_string ship] converts a ship to its string representation. *)

val get_ship_update : string -> ship list ref -> ship
(** [get_ship_update name ships] gets the updated ship from the given list of
    ships by its name. *)

val get_comp_hits : unit -> int
(** [get_comp_hits ()] returns the total number of hits on all computer ships. *)
