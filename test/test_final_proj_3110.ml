open OUnit2

(*open QCheck2 *)
(*open Final_proj_3110.Computer*)
open Final_proj_3110.Board

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
  QCheck2.Test.make ~count:100 ~name:"randomly generated let orientations"
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
  new_random_board; new_random_board; ]

  (* [garden_tests array] QCheck tests that check for whether the garden is
  incremented correctly and if size stays consistence *) let garden_tests (array
  : A5.Plant.t array array) = (* Check correct increment *) let rows =
  Array.length array in let cols = Array.length array.(0) in let
  test_incr_garden = init_garden rows cols in let iterated_incr_garden =
  incr_garden test_incr_garden rows cols in let tot_garden_age_incr =
  total_garden_age iterated_incr_garden rows cols in let incr_consis_test =
  tot_garden_age_incr >= 0 in (* Check size consistency *) let test_size_garden
  = init_garden rows cols in let iterated_size_garden = incr_garden
  test_size_garden rows cols in let original_row = rows in let original_col =
  cols in let new_row = get_rows iterated_size_garden in let new_col = get_cols
  iterated_size_garden in let size_consis_test = original_row = new_row &&
  original_col = new_col in (* Return the combined result of all tests *)
  size_consis_test && incr_consis_test

  (* [random_tests] tests [garden_tests] with random gardens *) let random_tests
  = QCheck2.Test.make ~count:1000 ~name:"random tests" garden_gen garden_tests

  (* Converts QCheck tests into OUnit test *) let ounit_random_test =
  QCheck_runner.to_ounit2_test random_tests

  (* OUnit Tests *) let empty_garden = init_garden 2 2

  (* tests to_string *) let test_to_string_empty = "tests to_string" >:: fun _
  -> assert_equal ~printer:(fun x -> x) " " (to_string empty_garden 2 2)

  (* tests [incr_garden]*) let incr_non_empty = incr_garden (pop_non_empty
  (init_garden 2 2) 2 2) 2 2

  (* another test for [incr_garden]*) let test_incr_garden_2 = "tests
  incr_garden" >:: fun _ -> assert_equal ~printer:(fun x -> x) "🌱🌱🌱🌱" (to_string
  incr_non_empty 2 2)

  (* iterates garden [int] times*) let rec iter_garden garden int = if int > 0
  then iter_garden (incr_garden garden 2 2) (int - 1) else garden

  (* garden at oldest age *) let death_garden = iter_garden (pop_non_empty
  (init_garden 2 2) 2 2) 12

  (* checks if garden dies properly *) let test_plant_death = "tests
  incr_garden" >:: fun _ -> assert_equal ~printer:(fun x -> x) " " (to_string
  death_garden 2 2) *)

let test_orientation =
  "test suite for orientation"
  >::: [ ounit_test_left; ounit_test_right; ounit_test_down; ounit_test_up ]

(*check ships coords and add_coords tests*)

let new_board = create_board ()
let lst_exist = [ (1, 2); (8,8) ] (*list for checking existing coords*)
let lst_nexist = [ (4, 4); (7, 7) ] (*list for checking not existing coords*)
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

(*set board and match ship tests*)

let lst_b = [ (4,4); (5,4); (6,4) ] 
let _ = set_board new_board lst_b 3 3
let lst_c = [ (1,1); (1,2); (1,3) ] 
let _ = set_board new_board lst_c 31 3
let lst_d = [ (1,7); (2,7); (3,7); (4,7)] 
let _ = set_board new_board lst_d 4 4

let lst_e = [ (8,2); (8,3); (8,4); (8,5); (8,6)] 
let _ = set_board new_board lst_e 5 5
let test_set_board = 
  "tests set_board and match ship"
  >::: [
         ( "check that coords are set on the board for ship type a beginning" >:: fun _ ->
           assert_equal " a " (get_board_element new_board 7 7) );
         ( "check that coords are set on the board for ship type a end" >:: fun _ ->
           assert_equal " a " (get_board_element new_board 7 8) );
         ( "check that coords are set on the board for ship type b beginning" >:: fun _ ->
            assert_equal " b " (get_board_element new_board 3 3) );
         ( "check that coords are set on the board for ship type b end" >:: fun _ ->
            assert_equal " b " (get_board_element new_board 5 3) );
         ( "check that coords are set on the board for ship type c beginning" >:: fun _ ->
            assert_equal " c " (get_board_element new_board 0 0) );
         ( "check that coords are set on the board for ship type c end" >:: fun _ ->
            assert_equal " c " (get_board_element new_board 0 2) );
         ( "check that coords are set on the board for ship type d beginning" >:: fun _ ->
            assert_equal " d " (get_board_element new_board 0 6) );
         ( "check that coords are set on the board for ship type d end" >:: fun _ ->
            assert_equal " d " (get_board_element new_board 3 6) );
         ( "check that coords are set on the board for ship type e beginning" >:: fun _ ->
            assert_equal " e " (get_board_element new_board 7 1) );
         ( "check that coords are set on the board for ship type e end" >:: fun _ ->
            assert_equal " e " (get_board_element new_board 7 5) );
       ]


let _ = run_test_tt_main test_orientation
let _ = run_test_tt_main test_check_ship_coord
let _ = run_test_tt_main test_set_board
