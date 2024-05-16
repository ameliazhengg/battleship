open OUnit2

(*open QCheck2 *)
open Final_proj_3110.Computer
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
         >:: fun _ ->
           assert_equal "\x1b[38;5;215m a \027[0m"
             (get_board_element new_board 7 7) );
         ( "check that coords are set on the board for ship type a end"
         >:: fun _ ->
           assert_equal "\x1b[38;5;215m a \027[0m"
             (get_board_element new_board 7 8) );
         ( "check that coords are set on the board for ship type b beginning"
         >:: fun _ ->
           assert_equal "\x1b[38;5;216m b \027[0m"
             (get_board_element new_board 3 3) );
         ( "check that coords are set on the board for ship type b end"
         >:: fun _ ->
           assert_equal "\x1b[38;5;216m b \027[0m"
             (get_board_element new_board 5 3) );
         ( "check that coords are set on the board for ship type c beginning"
         >:: fun _ ->
           assert_equal "\x1b[38;5;219m c \027[0m"
             (get_board_element new_board 0 0) );
         ( "check that coords are set on the board for ship type c end"
         >:: fun _ ->
           assert_equal "\x1b[38;5;219m c \027[0m"
             (get_board_element new_board 0 2) );
         ( "check that coords are set on the board for ship type d beginning"
         >:: fun _ ->
           assert_equal "\x1b[38;5;217m d \027[0m"
             (get_board_element new_board 0 6) );
         ( "check that coords are set on the board for ship type d end"
         >:: fun _ ->
           assert_equal "\x1b[38;5;217m d \027[0m"
             (get_board_element new_board 3 6) );
         ( "check that coords are set on the board for ship type e beginning"
         >:: fun _ ->
           assert_equal "\x1b[38;5;218m e \027[0m"
             (get_board_element new_board 7 1) );
         ( "check that coords are set on the board for ship type e end"
         >:: fun _ ->
           assert_equal "\x1b[38;5;218m e \027[0m"
             (get_board_element new_board 7 5) );
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
         ( "find_ship_name a" >:: fun _ ->
           assert_equal 2 (find_ship_name "\x1b[38;5;210m a \027[0m") );
         ( "find_ship_name b" >:: fun _ ->
           assert_equal 3 (find_ship_name "\x1b[38;5;216m b \027[0m") );
         ( "find_ship_name c" >:: fun _ ->
           assert_equal 31 (find_ship_name "\x1b[38;5;217m c \027[0m") );
         ( "find_ship_name d" >:: fun _ ->
           assert_equal 4 (find_ship_name "\x1b[38;5;218m d \027[0m") );
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
           assert_equal c_ship
             (get_ship_update "\x1b[38;5;217m c \027[0m" hit_user_ships) );
       ]

(*Logic Tests*)
let _ = add_user_guess (1, 1)
let _ = add_user_guess (9, 10)
let _ = add_user_guess (10, 9)
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

let test_computer_logic =
  "test Logic for computer logic functions"
  >::: [
         ( "convert_coords_to_game_format" >:: fun _ ->
           assert_equal
             [ (1, 'A'); (4, 'E') ]
             (convert_coords_to_game_format [ (1, 1); (4, 5) ]) );
         ( "string_of_list_coords" >:: fun _ ->
           assert_equal "1,A; 4,E" (string_of_list_coords [ (1, 1); (4, 5) ]) );
         ( "is_on_board_edge first row" >:: fun _ ->
           assert_equal true (is_on_board_edge 1 8) );
         ( "is_on_board_edge first row" >:: fun _ ->
           assert_equal true (is_on_board_edge 1 10) );
         ( "is_on_board_edge last row" >:: fun _ ->
           assert_equal true (is_on_board_edge 10 8) );
         ( "is_on_board_edge last row" >:: fun _ ->
           assert_equal true (is_on_board_edge 10 1) );
         ( "is_on_board_edge first column" >:: fun _ ->
           assert_equal true (is_on_board_edge 5 1) );
         ( "is_on_board_edge first square" >:: fun _ ->
           assert_equal true (is_on_board_edge 1 1) );
         ( "is_on_board_edge last column" >:: fun _ ->
           assert_equal true (is_on_board_edge 5 10) );
         ( "is_on_board_edge last square" >:: fun _ ->
           assert_equal true (is_on_board_edge 10 10) );
         ( "is_on_board_edge false" >:: fun _ ->
           assert_equal false (is_on_board_edge 5 5) );
         ( "filter_valid_coords" >:: fun _ ->
           assert_equal
             [ (1, 2); (2, 2) ]
             (filter_valid_coords [ (1, 2); (2, 2); (1, 1) ]) );
         ( "get_corner_coords top left corner" >:: fun _ ->
           assert_equal [ (1, 2); (2, 1) ] (get_corner_coords 1 1) );
         ( "get_corner_coords top right corner" >:: fun _ ->
           assert_equal [ (1, 9); (2, 10) ] (get_corner_coords 1 10) );
         ( "get_corner_coords bottom left corner" >:: fun _ ->
           assert_equal [ (10, 2); (9, 1) ] (get_corner_coords 10 1) );
         ( "get_corner_coords bottom right corner" >:: fun _ ->
           assert_equal [ (10, 9); (9, 10) ] (get_corner_coords 10 10) );
         ( "get_corner_coords not corner" >:: fun _ ->
           assert_equal [] (get_corner_coords 5 5) );
         ( "get_edge coordinates top border" >:: fun _ ->
           assert_equal [ (1, 7); (1, 5); (2, 6) ] (get_edge_coords 1 6) );
         ( "get_edge coordinates left border" >:: fun _ ->
           assert_equal [ (7, 2); (8, 1); (6, 1) ] (get_edge_coords 7 1) );
         ( "get_edge coordinates right border" >:: fun _ ->
           assert_equal [ (5, 9); (6, 10); (4, 10) ] (get_edge_coords 5 10) );
         ( "get_edge coordinates bottom border" >:: fun _ ->
           assert_equal [ (10, 5); (10, 3); (9, 4) ] (get_edge_coords 10 4) );
         ( "get_edge coordinates not on border" >:: fun _ ->
           assert_equal [ (6, 5); (6, 3); (7, 4); (5, 4) ] (get_edge_coords 6 4)
         );
       ]

let test_get_rec_coords_user _ =
  let corners = [ (1, 1); (10, 10); (1, 10); (10, 1) ] in
  List.iter
    (fun (row, col) ->
      assert_equal
        ~printer:(fun coords -> string_of_list_coords coords)
        (get_corner_coords row col)
        (get_rec_coords_user row col))
    corners;

  let edges = [ (1, 2); (10, 9); (2, 1); (9, 10) ] in
  List.iter
    (fun (row, col) ->
      assert_equal
        ~printer:(fun coords -> string_of_list_coords coords)
        (get_edge_coords row col |> filter_valid_coords)
        (get_rec_coords_user row col))
    edges;

  let inside_board = [ (5, 5); (6, 7) ] in
  List.iter
    (fun (row, col) ->
      assert_equal
        ~printer:(fun coords -> string_of_list_coords coords)
        (get_edge_coords row col |> filter_valid_coords)
        (get_rec_coords_user row col))
    inside_board

let suite =
  "Test for get recomended coordinates"
  >::: [ "test_get_rec_coords_user" >:: test_get_rec_coords_user ]

let () = run_test_tt_main suite

let test_remove_recommended_empty _ =
  rec_guesses := [];
  remove_recommended ();
  assert_equal [] !rec_guesses

let test_remove_recommended_one_item _ =
  rec_guesses := [];
  rec_guesses := [ (1, 2) ];
  remove_recommended ();
  assert_equal [] !rec_guesses

let test_remove_recommended_multiple_items _ =
  rec_guesses := [];
  rec_guesses := [ (1, 2); (3, 4); (5, 6) ];
  remove_recommended ();
  assert_equal [ (3, 4); (5, 6) ] !rec_guesses

let suite =
  "Test Suite for remove_recommended"
  >::: [
         "remove from empty" >:: test_remove_recommended_empty;
         "remove one item" >:: test_remove_recommended_one_item;
         "remove from multiple items" >:: test_remove_recommended_multiple_items;
       ]

let () = run_test_tt_main suite

let _ =
  let rec print lst =
    match lst with
    | [] -> () (* If the list is empty, do nothing *)
    | (x, y) :: tail ->
        Printf.printf "(%d, %d)\n" x y;
        (* Print the current pair *)
        print tail (* Recursively print the rest of the list *)
  in
  print (get_edge_coords 5 6)

(*Computer Tests*)

let test_create_computer_board _ =
  let board = create_computer_board () in
  assert_equal 10 (Array.length board);
  assert_equal 10 (Array.length board.(0));
  assert_equal "   " board.(0).(0);
  assert_equal false (0 = Array.length board);
  assert_equal false (0 = Array.length board.(0));
  assert_equal false ("  " = board.(0).(0));
  assert_equal false (11 = Array.length board);
  assert_equal false (11 = Array.length board.(0));
  assert_equal false ("  a  " = board.(0).(0));
  assert_equal false (2 = Array.length board);
  assert_equal false (2 = Array.length board.(0));
  assert_equal false ("  3  " = board.(0).(0))

(*let test_get_comp_board_element _ = let board = create_computer_board () in
  board.(0).(0) <- " a "; assert_equal " a " (get_comp_board_element board 0 0);
  let board1 = create_computer_board () in board1.(0).(0) <- " b "; assert_equal
  " b " (get_comp_board_element board 0 0); let board2 = create_computer_board
  () in board2.(9).(9) <- " b "; assert_equal " b " (get_comp_board_element
  board 9 9); let board3 = create_computer_board () in board3.(0).(9) <- " a ";
  assert_equal " a " (get_comp_board_element board 0 9); let board4 =
  create_computer_board () in board4.(9).(0) <- " b "; assert_equal " b "
  (get_comp_board_element board 9 0); let board5 = create_computer_board () in
  board5.(4).(5) <- " b "; board5.(0).(0) <- " b "; board5.(0).(1) <- " b ";
  board5.(0).(9) <- " b "; board5.(1).(9) <- " b "; board5.(9).(0) <- " b ";
  board5.(9).(9) <- " b "; board5.(2).(7) <- " b "; assert_equal " b "
  (get_comp_board_element board 4 5); assert_equal " b " (get_comp_board_element
  board 0 0); assert_equal " b " (get_comp_board_element board 0 1);
  assert_equal " b " (get_comp_board_element board 0 9); assert_equal " b "
  (get_comp_board_element board 1 9); assert_equal " b " (get_comp_board_element
  board 9 0); assert_equal " b " (get_comp_board_element board 9 9);
  assert_equal " b " (get_comp_board_element board 2 7) *)

let test_check_contains _ =
  assert_equal false (check_contains 1 0 3 [ 11 ]);
  assert_equal true (check_contains 1 0 2 [ 2 ]);
  assert_equal false (check_contains 20 1 3 [ 10 ]);
  assert_equal true (check_contains 12 1 3 [ 1 ]);
  assert_equal false (check_contains 11 2 3 [ 10 ]);
  assert_equal true (check_contains 5 2 3 [ 6 ]);
  assert_equal false (check_contains 1 3 3 [ 3 ]);
  assert_equal true (check_contains 10 3 3 [ 1 ]);
  assert_equal true (check_contains 10 3 3 [ 1; 2; 3 ]);
  assert_equal false (check_contains 2 3 3 [ 1; 2; 3 ]);
  assert_equal true (check_contains 2 3 3 []);
  assert_equal true (check_contains 2 2 3 []);
  assert_equal true (check_contains 2 1 3 []);
  assert_equal true (check_contains 2 0 3 []);
  assert_equal false (check_contains 100 2 3 [ 99; 98 ]);
  assert_equal false (check_contains 1 3 5 [ 1; 2; 3 ]);
  assert_equal false
    (check_contains 1 3 3
       [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17 ]);
  assert_equal false
    (check_contains 2 0 3
       [ 2; 3; 4; 5; 6; 7; 8; 9; 10; 12; 13; 14; 15; 16; 17 ]);
  assert_equal true
    (check_contains 91 1 3
       [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17 ]);
  assert_equal true
    (check_contains 50 0 3
       [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17 ])

let test_valid_placement =
  "valid_placement tests"
  >::: [
         ( "invalid >=91 0" >:: fun _ ->
           assert_equal false (valid_placement 91 0 3 []) );
         ( "invalid 80 0 3" >:: fun _ ->
           assert_equal false (valid_placement 80 0 3 []) );
         ( "invalid <=10 1" >:: fun _ ->
           assert_equal false (valid_placement 9 1 3 []) );
         ( "invalid 10 1 1" >:: fun _ ->
           assert_equal false (valid_placement 10 1 1 []) );
         ( "invalid mod 10 2" >:: fun _ ->
           assert_equal false (valid_placement 21 2 1 []) );
         ( "invalid 11 2 2" >:: fun _ ->
           assert_equal false (valid_placement 11 2 2 []) );
         ( "invalid mod 10 3" >:: fun _ ->
           assert_equal false (valid_placement 10 3 1 []) );
         ( "invalid 18 3 3" >:: fun _ ->
           assert_equal false (valid_placement 18 3 3 []) );
         ("valid 0" >:: fun _ -> assert_equal true (valid_placement 1 0 2 [ 2 ]));
         ( "valid 1" >:: fun _ ->
           assert_equal true (valid_placement 50 1 3 [ 49 ]) );
         ( "valid 2" >:: fun _ ->
           assert_equal true (valid_placement 24 2 3 [ 20 ]) );
         ( "valid 3" >:: fun _ ->
           assert_equal true (valid_placement 15 3 3 [ 12 ]) );
       ]

let test_random_coord _ =
  let coord = random_coord () in
  assert_bool "Coordinate should be between 1 and 100"
    (coord >= 1 && coord <= 100)

(*let test_add_ship_to_lst _ = occupied_coords := []; comp_ship_coords := [];
  add_ship_to_lst 2 1 0 3 []; assert_equal [ (2, 1); (2, 2); (2, 3) ]
  !comp_ship_coords; assert_equal [ 1; 11; 21 ] !occupied_coords

  let test_get_comp_lst_size _ = assert_equal 0 (get_comp_lst_size ()) *)

let test_in_comp_shi_coords _ =
  occupied_coords := [ 1; 11; 21 ];
  assert_equal true (in_comp_shi_coords 1 1);
  assert_equal false (in_comp_shi_coords 2 2)

let test_generate_random_guess _ =
  let row, col = generate_random_guess "easy" in
  assert_bool "Row should be between 1 and 10" (row >= 1 && row <= 10);
  assert_bool "Column should be between 1 and 10" (col >= 1 && col <= 10)

let suite =
  "Computer Test Suite"
  >::: [
         (*("test_match_ship a" >:: fun _ -> assert_equal " a " (ship_match 2));
           ("test_match_ship b" >:: fun _ -> assert_equal " b " (ship_match 3));
           ("test_match_ship c" >:: fun _ -> assert_equal " c " (ship_match
           31)); ("test_match_ship d" >:: fun _ -> assert_equal " d "
           (ship_match 4)); ("test_match_ship e" >:: fun _ -> assert_equal " e "
           (ship_match 5)); *)
         ( "test_match_ship anything else" >:: fun _ ->
           assert_equal "  " (ship_match 0) );
         "test_create_computer_board" >:: test_create_computer_board;
         (*"test_get_comp_board_element" >:: test_get_comp_board_element; *)
         "test_check_contains" >:: test_check_contains;
         "test_random_coord" >:: test_random_coord;
         (*"test_add_ship_to_lst" >:: test_add_ship_to_lst;
           "test_get_comp_lst_size" >:: test_get_comp_lst_size;*)
         "test_in_comp_shi_coords" >:: test_in_comp_shi_coords;
         "test_generate_random_guess" >:: test_generate_random_guess;
       ]

let _ = run_test_tt_main test_set_board
let _ = run_test_tt_main test_orientation
let _ = run_test_tt_main test_check_ship_coord
let _ = run_test_tt_main test_add_user_ship
let _ = run_test_tt_main test_create_coord_array
let _ = run_test_tt_main test_ships
let _ = run_test_tt_main test_logic
let _ = run_test_tt_main suite
let _ = run_test_tt_main test_valid_placement
let _ = run_test_tt_main test_computer_logic
let _ = run_test_tt_main test_generate_coords

(* tests generate hard guess *)
let rows = [| 1;2;3;4;5;6;7;8;9;10|]
let columns = [| 1;2;3;4;5;6;7;8;9;10|]
let recommended = ref [ (2, 2); (3, 3) ]

let test_easy_mode _ =
  let row, col = generate_random_guess "easy" in
  assert_bool "Valid row" (row >= 1 && row <= Array.length rows);
  assert_bool "Valid column" (col >= 1 && col <= Array.length columns)

let () = let r, c = generate_random_guess "easy" in print_endline (string_of_int r ^ string_of_int c )

let test_hard_no_recommendations _ =
  recommended := [];
  let row, col = generate_random_guess "hard" in
  assert_bool "Valid row" (row >= 1 && row <= Array.length rows);
  assert_bool "Valid column" (col >= 1 && col < Array.length columns)

 (*let hard_test_with_recommendations _ =
  let recs = !rec_guesses in
  let expected = List.hd !recommended in
  let result = generate_random_guess "hard" in
  assert_equal expected result ~printer:(fun (r, c) ->
      Printf.sprintf "(%d, %d)" r c);
  assert_bool "Recommendation not removed"
    (not (List.mem expected !rec_guesses)) *)

(*let test_medium_with_recommendations _ =
  recommended := [ (1, 1); (4, 4) ];
  (* Assume these are strategic spots for medium mode *)
  let result = generate_random_guess "medium" in
  assert_bool "Invalid selection for medium mode with recommendations"
    (List.mem result !recommended) *)

let test_medium_no_recommendations _ =
  recommended := [];
  (* No recommendations available *)
  let row, col = generate_random_guess "medium" in
  assert_bool "Valid row" (row >= 1 && row <= 10);
  assert_bool "Valid column" (if row mod 2 = 0 then col >= 1 && col <=9 + 1 else col >= 2 && col <= 10)

let suite =
  "Test Suite for generate_random_guess across modes"
  >::: [
         "test_easy_no_recommendations" >:: test_easy_mode;
         "test_hard_no_recommendations" >:: test_hard_no_recommendations;
         (*"test_hard_with_recommendations" >:: hard_test_with_recommendations;
         "test_medium_with_recommendations" >:: test_medium_with_recommendations; *)
         "test_medium_no_recommendations" >:: test_medium_no_recommendations;
       ]

let () = run_test_tt_main suite

(*test getting first and second function*)

let sample_list = [ (1, 2); (3, 4); (5, 6) ]

let test_first_element_valid _ =
  assert_equal 1 (get_first_element sample_list 0);
  assert_equal 3 (get_first_element sample_list 1);
  assert_equal 5 (get_first_element sample_list 2)

let test_first_element_invalid _ =
  assert_raises (Failure "nth") (fun () -> get_first_element sample_list 3)

let test_second_element_valid _ =
  assert_equal 2 (get_second_element sample_list 0);
  assert_equal 4 (get_second_element sample_list 1);
  assert_equal 6 (get_second_element sample_list 2)

let test_second_element_invalid _ =
  assert_raises (Failure "nth") (fun () -> get_second_element sample_list 3)

let suite =
  "Test Suite for List Elements "
  >::: [
         "test_first_element_valid" >:: test_first_element_valid;
         "test_first_element_invalid" >:: test_first_element_invalid;
         "test_second_element_valid" >:: test_second_element_valid;
         "test_second_element_invalid" >:: test_second_element_invalid;
       ]

let () = run_test_tt_main suite
