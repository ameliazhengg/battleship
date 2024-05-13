open Ships

let () = Random.self_init ()

exception Foo of string
exception Bar of string

type board = string array array

let columns = [| 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' |]

let rows =
  [| "  1"; "  2"; "  3"; "  4"; "  5"; "  6"; "  7"; "  8"; "  9"; " 10" |]

let comp_ship_coords = ref []
let occupied_coords = ref []

(* icon representing the different ships on our board*)
let match_ship n =
  match n with
  | 2 -> " a "
  | 3 -> " b "
  | 31 -> " c "
  | 4 -> " d "
  | 5 -> " e "
  | _ -> "  "

(*let computer_ship_coords = ref []*)
let create_computer_board () = Array.make_matrix 10 10 "   "
let get_board_element board row col = board.(row).(col)
let orientation = [| "up"; "down"; "left"; "right" |]
let random_row _ = 1 + Random.int (Array.length rows)
let random_col _ = Array.get columns (1 + Random.int (Array.length columns))

(* [random_dir] is a random number betwen 0 and 3 inclusive, 0 represent down, 1
   represent up, 2 represent left, 3 represents right *)
(* let random_dir start = match start with | x when x = 1 -> Random.int 2 * 3 |
   x when x = 10 -> Random.int 2 * 2 | x when x = 91 -> if Random.bool () then 1
   else 3 | x when x = 100 -> if Random.bool () then 1 else 2 | _ -> 5 *)

(* let random_dir start = match start with | x when x = 1 -> 3 * Random.int 2 |
   x when x = 10 -> 2 * Random.int 2 | x when x = 91 -> if Random.bool () then 1
   else 3 | x when x = 100 -> if Random.bool () then 1 else 2 | _ -> 5 *)

let random_dir _ = Random.int 4

(* [check_contains] is true if the coordinates of a potential ship are already
   occupied otherwise false *)
let rec check_contains start_cord dir len lst =
  match dir with
  | 0 ->
      if len = 0 then false
      else
        List.mem start_cord lst
        || check_contains (start_cord + 10) dir (len - 1) lst
  | 1 ->
      if len = 0 then false
      else
        List.mem start_cord lst
        || check_contains (start_cord - 10) dir (len - 1) lst
  | 2 ->
      if len = 0 then false
      else
        List.mem start_cord lst
        || check_contains (start_cord - 1) dir (len - 1) lst
  | 3 ->
      if len = 0 then false
      else
        List.mem start_cord lst
        || check_contains (start_cord + 1) dir (len - 1) lst
  | _ -> true

(* [valid_placement] is true if a ship with [start_coord] [dir] and [len] can be
   placed there otherwise false *)
let valid_placement start_cord dir len =
  match dir with
  | 0 ->
      if start_cord >= 91 then false
      else if (start_cord / 10) + len > 10 then false
      else check_contains start_cord dir len !occupied_coords
  | 1 ->
      if start_cord <= 10 then false
      else if (start_cord / 10) - len < 0 then false
      else check_contains start_cord dir len !occupied_coords
  | 2 ->
      if start_cord mod 10 = 1 then false
      else if (start_cord mod 10) - len < 0 then false
      else check_contains start_cord dir len !occupied_coords
  | 3 ->
      if start_cord mod 10 = 0 then false
      else if (start_cord mod 10) + len > 10 then false
      else check_contains start_cord dir len !occupied_coords
  | _ -> false

(* [random_coord] is a random number between 1 and 100 inclusive representing
   the coordinate in a 10x10 grid *)
let rec random_coord occupied_coords_lst =
  let coord = 1 + Random.int 100 in
  (* Generate a random integer from 1 to 100 *)
  if List.mem coord !occupied_coords_lst then
    random_coord
      occupied_coords_lst (* If the coord is already occupied, try again *)
  else begin
    occupied_coords := !occupied_coords @ [ coord ];
    coord
  end

let rec add_ship_to_lst name start dir len lst =
  if len = 0 then add_computer_ship name (List.length lst) lst
  else begin
    comp_ship_coords := !comp_ship_coords @ [ (name, start) ];
    (* if List.mem start !occupied_coords then () else *)
    occupied_coords := !occupied_coords @ [ start ];
    let lst = lst @ [ (start / 10, start mod 10) ] in
    match dir with
    | 0 -> add_ship_to_lst name (start + 10) 0 (len - 1) lst
    | 1 -> add_ship_to_lst name (start - 10) 1 (len - 1) lst
    | 2 -> add_ship_to_lst name (start - 1) 2 (len - 1) lst
    | 3 -> add_ship_to_lst name (start + 1) 3 (len - 1) lst
    | _ -> ()
  end

let rec new_ship_coord start dir len name =
  if len = 6 then new_ship_coord start dir 3 31
  else if valid_placement start dir len = true then
    add_ship_to_lst name start dir len []
  else
    let new_start = random_coord occupied_coords in
    new_ship_coord new_start (random_dir new_start) len name

let add_coords =
  for i = 2 to 6 do
    let new_rand = random_coord occupied_coords in
    new_ship_coord new_rand (random_dir new_rand) i i
  done

let random_board board =
  List.iter
    (fun (name, coord) ->
      let row = coord / 10 in
      let col = (coord mod 10) - 1 in
      let icon = match_ship name in
      board.(row).(col) <- icon)
    !comp_ship_coords;
  board

(* let random_board board = board *)
let get_comp_lst_size = List.length !comp_ship_coords

let rec com_lst_to_string lst =
  match lst with
  | [] -> ""
  | h :: t ->
      string_of_int (fst h)
      ^ ", "
      ^ string_of_int (snd h)
      ^ " | " ^ com_lst_to_string t

let rec occ_lst_to_string lst =
  match lst with
  | [] -> ""
  | h :: t -> string_of_int h ^ " | " ^ occ_lst_to_string t

let string_comp_ships = com_lst_to_string !comp_ship_coords
let string_occ_coord = occ_lst_to_string !occupied_coords
