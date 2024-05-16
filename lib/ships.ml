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

let is_sunk ship = ship.hits >= ship.length - 1

let check_all_hit ships =
  if List.fold_left (fun acc x -> acc + x.hits) 0 !ships >= 16 then true
  else false

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
  | "\x1b[38;5;210m a \027[0m" -> 2
  | "\x1b[38;5;4m a \027[0m" -> 2
  | "\x1b[38;5;168m a \027[0m" -> 2
  | "\x1b[38;5;21m a \027[0m" -> 2
  | "\x1b[38;5;172m a \027[0m" -> 2
  | "\x1b[38;5;214m a \027[0m" -> 2
  | "\x1b[38;5;216m b \027[0m" -> 3
  | "\x1b[38;5;6m b \027[0m" -> 3
  | "\x1b[38;5;169m b \027[0m" -> 3
  | "\x1b[38;5;26m b \027[0m" -> 3
  | "\x1b[38;5;215m b \027[0m" -> 3
  | "\x1b[38;5;218m b \027[0m" -> 3
  | "\x1b[38;5;217m c \027[0m" -> 31
  | "\x1b[38;5;30m c \027[0m" -> 31
  | "\x1b[38;5;170m c \027[0m" -> 31
  | "\x1b[38;5;25m c \027[0m" -> 31
  | "\x1b[38;5;216m c \027[0m" -> 31
  | "\x1b[38;5;31m c \027[0m" -> 31
  | "\x1b[38;5;218m d \027[0m" -> 4
  | "\x1b[38;5;183m d \027[0m" -> 4
  | "\x1b[38;5;171m d \027[0m" -> 4
  | "\x1b[38;5;24m d \027[0m" -> 4
  | "\x1b[38;5;214m d \027[0m" -> 4
  | "\x1b[38;5;32m d \027[0m" -> 4
  | _ -> 5

let find_ship_in_list ships name =
  List.find (fun ship -> ship.name = name) ships

let get_comp_ships () = computer_ships
let get_user_ships () = user_ships

let ship_to_string ship =
  "name: " ^ string_of_int ship.name ^ " | " ^ "length: "
  ^ string_of_int ship.length ^ " | " ^ "hits: " ^ string_of_int ship.hits
  ^ " | "

let get_ship_update ship_rep ships =
  let ship_name = find_ship_name ship_rep in
  let ship = find_ship_in_list !ships ship_name in
  update_ship_hit ships ship_name;
  ship

let get_comp_hits () =
  List.fold_left (fun acc x -> acc + x.hits) 0 !computer_ships
