type ship = {
  name : int;
  length : int;
  coordinates : (int * int) list;
  hits : int;
}

type coord_list = (int * int) list

val user_ships : ship list ref
val computer_ships : ship list ref
val get_length : ship -> string
val add_computer_ship : int -> int -> coord_list -> unit
val add_user_ship : int -> int -> coord_list -> unit
val is_sunk : ship -> bool
val check_all_hit : ship list ref -> bool
val update_ship_hit : ship list ref -> int -> unit
val find_ship_name : string -> int
val find_ship_in_list : ship list -> int -> ship
val get_comp_ships : unit -> ship list ref
val ship_to_string : ship -> string
