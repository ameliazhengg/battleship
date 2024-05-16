open OUnit2

(*open QCheck2 *)
(*open Final_proj_3110.Computer *)
open Final_proj_3110.Ships
open Final_proj_3110.Board
open Final_proj_3110.Logic

(*Board tests*)

(*check orientation tests*)
let generator = QCheck2.Gen.(1 -- 9)
let generator2 = QCheck2.Gen.(2 -- 10)

let test_orientation_left col =
  check_orientation ( - ) col 2 && check_orientation ( - ) 1 2 = false

let test_orientation_right col =
  check_orientation ( + ) col 2 && check_orientation ( + ) 10 2 = false

let test_orientation_down row =
  check_orientation ( + ) row 2 && check_orientation ( + ) 10 2 = false

let test_orientation_up row =
  check_orientation ( - ) row 2 && check_orientation ( - ) 1 2 = false

let test_left =
  QCheck2.Test.make ~count:100 ~name:"randomly generated left orientations"
    generator2 test_orientation_left

let test_right =
  QCheck2.Test.make ~count:100 ~name:"randomly generated right orientations"
    generator test_orientation_right

let test_down =
  QCheck2.Test.make ~count:100 ~name:"randomly generated down orientations"
    generator test_orientation_down

let test_up =
  QCheck2.Test.make ~count:100 ~name:"randomly generated up orientations"
    generator2 test_orientation_up

let ounit_test_left = QCheck_runner.to_ounit2_test test_left
let ounit_test_right = QCheck_runner.to_ounit2_test test_right
let ounit_test_down = QCheck_runner.to_ounit2_test test_down
let ounit_test_up = QCheck_runner.to_ounit2_test test_up

let test_orientation =
  "test suite for orientation"
  >::: [ ounit_test_left; ounit_test_right; ounit_test_down; ounit_test_up ]

(*let board = create_board () in let lst = [(row, col)] in let set = set_board
  board lst 3 3 in let Garden.num_rows garden = l && Array.length
  (Garden.get_row 0 garden) = w *)

(* [plant_generator] generates a random plant at a random stage in life *)
(*let board_generator = let open Final_proj_3110.Computer in Gen.oneofl [
  new_random_board; new_random_board; new_random_board; new_random_board;
  new_random_board; new_random_board; new_random_board; new_random_board;
  new_random_board; new_random_board; new_random_board; new_random_board;
  new_random_board; new_random_board; new_random_board; new_random_board;
  new_random_board; new_random_board; new_random_board; new_random_board;
  new_random_board; new_random_board; ] *)

(*check ships coords and add_coords tests*)

let new_board = create_board ()
let lst_exist = [ (1, 2); (8, 8) ] (*list for checking existing coords*)
let lst_nexist = [ (4, 5); (7, 7) ] (*list for checking not existing coords*)
let lst_a = [ (8, 8); (8, 9) ] (*list of existing coords*)
let _ = check_ships_coord new_board lst_a 2 2

let test_check_ship_coord =
  "tests check_ships_coord"
  >::: [
         ( "coordinates already exist" >:: fun _ ->
           assert_equal false (check_ships_coord new_board lst_exist 2 2) );
         ( "coordinates do not already exist" >:: fun _ ->
           assert_equal true (check_ships_coord new_board lst_nexist 2 2) );
         ( "check that coords are added if they do not exist" >:: fun _ ->
           assert_equal true (List.mem (8, 8) !user_ship_coords) );
       ]

(*add_user_ship tests*)
let a_ship =
  { name = 2; length = 2; coordinates = [ (8, 8); (8, 9) ]; hits = 0 }

let b_ship =
  { name = 3; length = 3; coordinates = [ (4, 4); (5, 4); (6, 4) ]; hits = 3 }

let test_add_user_ship =
  "test add_user_ship"
  >::: [
         ( "a ship is in the list" >:: fun _ ->
           assert_equal true (List.mem a_ship !user_ships) );
         ( "a ship is not in the list" >:: fun _ ->
           assert_equal false (List.mem b_ship !user_ships) );
       ]

(*set board and match ship tests*)

let lst_b = [ (4, 4); (5, 4); (6, 4) ]
let _ = set_board new_board lst_b 3 3
let lst_c = [ (1, 1); (1, 2); (1, 3) ]
let _ = set_board new_board lst_c 31 3
let lst_d = [ (1, 7); (2, 7); (3, 7); (4, 7) ]
let _ = set_board new_board lst_d 4 4
let lst_e = [ (8, 2); (8, 3); (8, 4); (8, 5); (8, 6) ]
let _ = set_board new_board lst_e 5 5

let test_set_board =
  "tests set_board and match ship"
  >::: [
         ( "check that coords are set on the board for ship type a beginning"
         >:: fun _ -> assert_equal " a " (get_board_element new_board 7 7) );
         ( "check that coords are set on the board for ship type a end"
         >:: fun _ -> assert_equal " a " (get_board_element new_board 7 8) );
         ( "check that coords are set on the board for ship type b beginning"
         >:: fun _ -> assert_equal " b " (get_board_element new_board 3 3) );
         ( "check that coords are set on the board for ship type b end"
         >:: fun _ -> assert_equal " b " (get_board_element new_board 5 3) );
         ( "check that coords are set on the board for ship type c beginning"
         >:: fun _ -> assert_equal " c " (get_board_element new_board 0 0) );
         ( "check that coords are set on the board for ship type c end"
         >:: fun _ -> assert_equal " c " (get_board_element new_board 0 2) );
         ( "check that coords are set on the board for ship type d beginning"
         >:: fun _ -> assert_equal " d " (get_board_element new_board 0 6) );
         ( "check that coords are set on the board for ship type d end"
         >:: fun _ -> assert_equal " d " (get_board_element new_board 3 6) );
         ( "check that coords are set on the board for ship type e beginning"
         >:: fun _ -> assert_equal " e " (get_board_element new_board 7 1) );
         ( "check that coords are set on the board for ship type e end"
         >:: fun _ -> assert_equal " e " (get_board_element new_board 7 5) );
         ( "check that other oords are set on the board are empty" >:: fun _ ->
           assert_equal "   " (get_board_element new_board 5 5) );
       ]

(*generate_coords tests*)

let pair_generator = QCheck2.Gen.(pair (1 -- 10) (3 -- 8))
let pair_generator2 = QCheck2.Gen.(pair (3 -- 8) (1 -- 10))

let test_coord_left (r, c) =
  [ (r, c); (r, c - 1); (r, c - 2) ] = generate_coords [] 3 "left" r c

let test_coord_right (r, c) =
  [ (r, c); (r, c + 1); (r, c + 2) ] = generate_coords [] 3 "right" r c

let test_coord_down (r, c) =
  [ (r, c); (r + 1, c); (r + 2, c) ] = generate_coords [] 3 "down" r c

let test_coord_up (r, c) =
  [ (r, c); (r - 1, c); (r - 2, c) ] = generate_coords [] 3 "up" r c

let test_generate_left =
  QCheck2.Test.make ~count:20 ~name:"randomly generated left coordinates"
    pair_generator test_coord_left

let test_generate_right =
  QCheck2.Test.make ~count:20 ~name:"randomly generated right coordinates"
    pair_generator test_coord_right

let test_generate_down =
  QCheck2.Test.make ~count:20 ~name:"randomly generated down coordinates"
    pair_generator2 test_coord_down

let test_generate_up =
  QCheck2.Test.make ~count:20 ~name:"randomly generated up coordinates"
    pair_generator2 test_coord_up

let ounit_test_coord_left = QCheck_runner.to_ounit2_test test_generate_left
let ounit_test_coord_right = QCheck_runner.to_ounit2_test test_generate_right
let ounit_test_coord_down = QCheck_runner.to_ounit2_test test_generate_down
let ounit_test_coord_up = QCheck_runner.to_ounit2_test test_generate_up

let test_generate_coords =
  "test suite for orientation"
  >::: [
         ounit_test_coord_left;
         ounit_test_coord_right;
         ounit_test_coord_down;
         ounit_test_coord_up;
       ]

(*create_coord_array tests*)

let test_create_coord_array =
  "tests create_coord_array"
  >::: [
         ( "check valid ship" >:: fun _ ->
           assert_equal true (create_coord_array "right" 6 7 3 3 new_board) );
         ( "check invalid ship" >:: fun _ ->
           assert_equal false (create_coord_array "down" 7 8 3 3 new_board) );
       ]

(*let () = let rec print = function | [] -> () | (a,b) :: tl -> print_endline
  (string_of_int a ^ string_of_int b); print tl in print !user_ship_coords *)

(*Ship tests*)
let _ = check_ships_coord new_board lst_b 3 3
let _ = update_ship_hit user_ships 3

let hit_user_ships =
  ref
    [
      { name = 2; length = 2; coordinates = [ (8, 8); (8, 9) ]; hits = 2 };
      {
        name = 3;
        length = 3;
        coordinates = [ (4, 4); (5, 4); (6, 4) ];
        hits = 3;
      };
      {
        name = 31;
        length = 3;
        coordinates = [ (1, 1); (1, 2); (1, 3) ];
        hits = 3;
      };
      {
        name = 4;
        length = 4;
        coordinates = [ (1, 7); (2, 7); (3, 7); (4, 7) ];
        hits = 4;
      };
      {
        name = 5;
        length = 5;
        coordinates = [ (8, 2); (8, 3); (8, 4); (8, 5); (8, 6) ];
        hits = 5;
      };
    ]

let c_ship =
  { name = 31; length = 3; coordinates = [ (1, 1); (1, 2); (1, 3) ]; hits = 3 }

let test_ships =
  "test suite for Ships"
  >::: [
         ("get_length test" >:: fun _ -> assert_equal "3" (get_length b_ship));
         ("is_sunk test true" >:: fun _ -> assert_equal true (is_sunk b_ship));
         ("is_sunk test false" >:: fun _ -> assert_equal false (is_sunk a_ship));
         ( "update_ship_hit ship b" >:: fun _ ->
           assert_equal 1 (List.nth !user_ships 0).hits );
         ("find_ship_name a" >:: fun _ -> assert_equal 2 (find_ship_name " a "));
         ("find_ship_name b" >:: fun _ -> assert_equal 3 (find_ship_name " b "));
         ("find_ship_name c" >:: fun _ -> assert_equal 31 (find_ship_name " c "));
         ("find_ship_name d" >:: fun _ -> assert_equal 4 (find_ship_name " d "));
         ( "check_all_hit false" >:: fun _ ->
           assert_equal false (check_all_hit user_ships) );
         ( "check_all_ hit true" >:: fun _ ->
           assert_equal true (check_all_hit hit_user_ships) );
         ( "check find_ship_in_list in list" >:: fun _ ->
           assert_equal b_ship (find_ship_in_list !hit_user_ships 3) );
         ( "check find_ship_in_list not in list" >:: fun _ ->
           assert_raises Not_found (fun () ->
               find_ship_in_list !hit_user_ships 6) );
         ( "check ship_to_string" >:: fun _ ->
           assert_equal "name: 2 | length: 2 | hits: 0 | "
             (ship_to_string a_ship) );
         ( "check get_ship_update returns the ship" >:: fun _ ->
           assert_equal c_ship (get_ship_update " c " hit_user_ships) );
       ]

(*Logic Tests*)
let _ = add_user_guess (1, 1)
let _ = add_computer_guess (2, 2)
let _ = mark_on_board new_board (10, 10) " O "

let test_logic =
  "test suite for Logic"
  >::: [
         ( "check that guess exists in add_user_guess" >:: fun _ ->
           assert_equal true (List.mem (1, 1) !user_guesses) );
         ( "check that guess exists in add_computer_guess" >:: fun _ ->
           assert_equal true (List.mem (2, 2) !computer_guesses) );
         ( "valid_guess_user returns already guessed coordinate as false "
         >:: fun _ -> assert_equal false (valid_guess_user 1 1) );
         ( "valid_guess_user returns not guessed coordinate as true "
         >:: fun _ -> assert_equal true (valid_guess_user 2 2) );
         ( "valid_guess_computer returns already guessed coordinate as false "
         >:: fun _ -> assert_equal false (valid_guess_computer (2, 2)) );
         ( "valid_guess_computer returns not guessed coordinate as true "
         >:: fun _ -> assert_equal true (valid_guess_computer (1, 1)) );
         ( "mark_on_board changes the symbol on the board" >:: fun _ ->
           assert_equal " O " (get_board_element new_board 9 9) );
         ( "is_valid_row_input when input is not a number" >:: fun _ ->
           assert_equal false (is_valid_row_input "a") );
         ( "is_valid_row_input when input is greater than 10" >:: fun _ ->
           assert_equal false (is_valid_row_input "12") );
         ( "is_valid_row_input when input is less than 1" >:: fun _ ->
           assert_equal false (is_valid_row_input "0") );
         ( "is_valid_row_input when input is valid" >:: fun _ ->
           assert_equal true (is_valid_row_input "8") );
         ( "is_valid_col_input when input is valid" >:: fun _ ->
           assert_equal true (is_valid_col_input "G") );
         ( "is_valid_col_input when input has length longer than 1" >:: fun _ ->
           assert_equal false (is_valid_col_input " G ") );
         ( "is_valid_col_input when input is invalid" >:: fun _ ->
           assert_equal false (is_valid_col_input "Z") );
         ( "is_valid_col_input when input is invalid" >:: fun _ ->
           assert_equal false (is_valid_col_input "@") );
         ( "is_valid_col_input when input is invalid" >:: fun _ ->
           assert_equal false (is_valid_col_input "z") );
       ]

let _ = run_test_tt_main test_orientation
let _ = run_test_tt_main test_check_ship_coord
let _ = run_test_tt_main test_add_user_ship
let _ = run_test_tt_main test_set_board
let _ = run_test_tt_main test_generate_coords
let _ = run_test_tt_main test_create_coord_array
let _ = run_test_tt_main test_ships
let _ = run_test_tt_main test_logic
