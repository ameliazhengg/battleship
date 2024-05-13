type ship = {
  name : int;
  length : int;
  coordinates : (int * int) list;
  hits : int;
}

type coord_list = (int * int) list

let user_ships = ref []
let computer_ships = ref []

let add_computer_ship name length (coordinates : coord_list) =
  computer_ships := { name; length; coordinates; hits = 0 } :: !computer_ships

let add_user_ship name length (coordinates : coord_list) =
  user_ships := { name; length; coordinates; hits = 0 } :: !user_ships

(* check if ship is sunk *)
let is_sunk ship = ship.hits >= ship.length
let find_ship ships name = List.find_opt (fun ship -> ship.name = name) !ships

(*let output match_emoji = function | true -> "❌" | false -> "⚪️" *)
