open Ships
open Logic

let () = Random.self_init ()

type board = string array array

let columns = [| 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' |]

let rows =
  [| "  1"; "  2"; "  3"; "  4"; "  5"; "  6"; "  7"; "  8"; "  9"; " 10" |]

let themes_list =
  [
    [
      "\x1b[38;5;210m";
      "\x1b[38;5;216m";
      "\x1b[38;5;217m";
      "\x1b[38;5;218m";
      "\x1b[38;5;219m";
    ];
    (* Ultra Rosa *)
    [
      "\x1b[38;5;4m";
      "\x1b[38;5;6m";
      "\x1b[38;5;30m";
      "\x1b[38;5;183m";
      "\x1b[38;5;226m";
    ];
    (* Mango Loco *)
    [
      "\x1b[38;5;168m";
      "\x1b[38;5;169m";
      "\x1b[38;5;170m";
      "\x1b[38;5;171m";
      "\x1b[38;5;129m";
    ];
    (* Pipeline Punch *)
    [
      "\x1b[38;5;21m";
      "\x1b[38;5;26m";
      "\x1b[38;5;25m";
      "\x1b[38;5;24m";
      "\x1b[38;5;184m";
    ];
    (* Aussie Lemonade *)
    [
      "\x1b[38;5;172m";
      "\x1b[38;5;215m";
      "\x1b[38;5;216m";
      "\x1b[38;5;214m";
      "\x1b[38;5;218m";
    ];
    (* Papillon *)
    [
      "\x1b[38;5;214m";
      "\x1b[38;5;216m";
      "\x1b[38;5;31m";
      "\x1b[38;5;32m";
      "\x1b[38;5;220m";
    ];
    (* Khaotic *)
  ]

let comp_ship_coords = ref []
let occupied_coords = ref []
let theme = List.nth themes_list (Random.int 5)

let ship_match n =
  match n with
  | 2 -> List.nth theme 0 ^ " a " ^ "\x1b[0m"
  | 3 -> List.nth theme 1 ^ " b " ^ "\x1b[0m"
  | 31 -> List.nth theme 2 ^ " c " ^ "\x1b[0m"
  | 4 -> List.nth theme 3 ^ " d " ^ "\x1b[0m"
  | 5 -> List.nth theme 4 ^ " e " ^ "\x1b[0m"
  | _ -> "  "

let create_computer_board () = Array.make_matrix 10 10 "   "
let get_comp_board_element board row col = board.(row).(col)
let random_dir _ = Random.int 4

let rec check_contains start_cord dir len lst =
  match dir with
  | 0 ->
      if len = 0 then true
      else
        (not (List.mem start_cord lst))
        && check_contains (start_cord + 10) dir (len - 1) lst
  | 1 ->
      if len = 0 then true
      else
        (not (List.mem start_cord lst))
        && check_contains (start_cord - 10) dir (len - 1) lst
  | 2 ->
      if len = 0 then true
      else
        (not (List.mem start_cord lst))
        && check_contains (start_cord - 1) dir (len - 1) lst
  | 3 ->
      if len = 0 then true
      else
        (not (List.mem start_cord lst))
        && check_contains (start_cord + 1) dir (len - 1) lst
  | _ -> true

let valid_placement start_cord dir len lst =
  match dir with
  | 0 ->
      if start_cord >= 91 then false
      else if (start_cord / 10) + len > 10 then false
      else check_contains start_cord dir len lst
  | 1 ->
      if start_cord <= 10 then false
      else if (start_cord / 10) - len < 0 then false
      else check_contains start_cord dir len lst
  | 2 ->
      if start_cord mod 10 = 1 then false
      else if (start_cord mod 10) - len < 0 then false
      else check_contains start_cord dir len lst
  | 3 ->
      if start_cord mod 10 = 0 then false
      else if (start_cord mod 10) + len > 10 then false
      else check_contains start_cord dir len lst
  | _ -> false

let random_coord _ =
  let coord = 1 + Random.int 100 in
  coord

let rec add_ship_to_lst name start dir len lst =
  if len = 0 then add_computer_ship name (List.length lst) lst
  else begin
    occupied_coords := !occupied_coords @ [ start ];
    comp_ship_coords := !comp_ship_coords @ [ (name, start) ];
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
  else if valid_placement start dir len !occupied_coords = true then
    add_ship_to_lst name start dir len []
  else
    let new_start = random_coord occupied_coords in
    new_ship_coord new_start (random_dir new_start) len name

let add_coords () =
  for i = 2 to 6 do
    let new_rand = random_coord occupied_coords in
    new_ship_coord new_rand (random_dir new_rand) i i
  done

let rec row_col_to_string lst =
  match lst with
  | [] -> ""
  | h :: t ->
      string_of_int ((snd h - 1) / 10)
      ^ ", "
      ^ string_of_int ((snd h - 1) mod 10)
      ^ " | " ^ row_col_to_string t

let random_board board =
  add_coords ();
  List.iter
    (fun (name, coord) ->
      let row = if coord = 100 then 9 else coord / 10 in
      let col = (coord - 1) mod 10 in
      let icon = ship_match name in
      board.(row).(col) <- icon)
    !comp_ship_coords;
  board

let get_comp_lst_size () = List.length !comp_ship_coords

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
let string_row_col = row_col_to_string !comp_ship_coords

let rec make_occupied_coords coord_lst lst =
  match lst with
  | [] -> coord_lst
  | h :: t ->
      let row = (h / 10) + 1 in
      let col = if h / 10 = 10 then 9 else h mod 10 in
      let new_lst = (row, col) :: coord_lst in
      make_occupied_coords new_lst t

let get_occupied_coords _ = make_occupied_coords [] !occupied_coords

let in_comp_shi_coords row col =
  if List.mem (((row - 1) * 10) + col) !occupied_coords then true else false

let get_hard_guesses () =
  if size_of_recommended () = 0 then begin
    let random_row = 1 + Random.int (Array.length rows) in
    let random_col = 1 + Random.int (Array.length columns - 1) in
    (random_row, random_col)
  end
  else begin
    let guess = get_rec_1 () in
    remove_recommended ();
    guess
  end

let medium_gen row =
  if row mod 2 = 0 then (2 * Random.int 5) + 1 else (2 * Random.int 5) + 2

let generate_random_guess mode =
  match mode with
  | "easy" -> begin
      let random_row = 1 + Random.int (Array.length rows) in
      let random_col = 1 + Random.int (Array.length columns) in
      (random_row, random_col)
    end
  | "medium" ->
      let row = 1 + Random.int 10 in
      let col = medium_gen row in
      (row, col)
  | _ -> get_hard_guesses ()

let create_concealed_board () = Array.make_matrix 10 10 "   "

let populate_concealed_board board =
  let correct_coords = get_correct_user_guess () in
  let mark_hit (row, col) = board.(row - 1).(col - 1) <- " X " in
  List.iter mark_hit correct_coords;
  let incorrect_coords = get_incorrect_user_guess () in
  let mark_miss (row, col) = board.(row - 1).(col - 1) <- " O " in
  List.iter mark_miss incorrect_coords;
  board
