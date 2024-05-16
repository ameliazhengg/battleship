type ship = {
  name : int;
  length : int;
  coordinates : (int * int) list;
  hits : int;
}

let user_ships = ref []
let computer_ships = ref []
let get_length ship = string_of_int ship.length

let add_computer_ship name length coordinates =
  computer_ships := { name; length; coordinates; hits = 0 } :: !computer_ships

let add_user_ship name length coordinates =
  user_ships := { name; length; coordinates; hits = 0 } :: !user_ships

(* check if ship is sunk *)
let is_sunk ship = ship.hits >= ship.length - 1

(* check if all ships are hit and thus game ends*)
let check_all_hit ships =
  if List.fold_left (fun acc x -> acc + x.hits) 0 !ships >= 17 then true
  else false

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
  List.find (fun ship -> ship.name = name) ships

let get_comp_ships () = computer_ships
let get_user_ships () = user_ships

let ship_to_string ship =
  "name: " ^ string_of_int ship.name ^ " | " ^ "length: "
  ^ string_of_int ship.length ^ " | " ^ "hits: " ^ string_of_int ship.hits
  ^ " | "

(* find the ship that was hit and increase the hit count by 1*)

let get_ship_update ship_rep ships =
  let ship_name = find_ship_name ship_rep in
  let ship = find_ship_in_list !ships ship_name in
  update_ship_hit ships ship_name;
  ship

let get_comp_hits () =
  List.fold_left (fun acc x -> acc + x.hits) 0 !computer_ships
