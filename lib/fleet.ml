type 'a t = int array array

let new_fleet () = Array.make_matrix 10 10 0

let add_ship fleet coord =
  fleet.(fst (Coord.get_coord coord)).(snd (Coord.get_coord coord)) <- 1;
  (fleet, true)

let contains fleet coord =
  if fleet.(fst (Coord.get_coord coord)).(snd (Coord.get_coord coord)) = 1 then
    true
  else false

let to_string fleet =
  let str_list = ref [] in
  Array.iter
    (fun row ->
      Array.iter
        (fun elem -> str_list := (if elem = 1 then "1" else "0") :: !str_list)
        row)
    fleet;
  String.concat " " (List.rev !str_list)
