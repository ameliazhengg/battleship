type ship = {
  name : int;
  length : int;
  coordinates : (int * int) list;
  hits : int;
}

type coord_list = (int * int) list

let user_ships = ref []
let computer_ships = ref []
let get_length ship = string_of_int ship.length

let add_computer_ship name length (coordinates : coord_list) =
  computer_ships := { name; length; coordinates; hits = 0 } :: !computer_ships

let add_user_ship name length (coordinates : coord_list) =
  user_ships := { name; length; coordinates; hits = 0 } :: !user_ships

(* check if ship is sunk *)
let is_sunk ship = ship.hits >= ship.length

(* Update the hit count of a ship and return updated list of ships *)
let update_ship_hit ships name =
  let updated =
    List.map
      (fun ship ->
        if ship.name = name then { ship with hits = ship.hits + 1 } else ship)
      !ships
  in
  ships := updated

let find_ship_name n =
  match n with
  | " a " -> 2
  | " b " -> 3
  | " c " -> 31
  | " d " -> 4
  | _ -> 5

let find_ship_in_list ships name =
  List.find (fun ship -> ship.name = name) !ships
