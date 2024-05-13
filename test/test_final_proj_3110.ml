(* open OUnit2 open QCheck2 open Final_proj_3110.Computer

   let new_random_board = random_board (create_computer_board ())

   (* [plant_generator] generates a random plant at a random stage in life *)
   let board_generator = let open Final_proj_3110.Computer in Gen.oneofl [
   new_random_board; new_random_board; new_random_board; new_random_board;
   new_random_board; new_random_board; new_random_board; new_random_board;
   new_random_board; new_random_board; new_random_board; new_random_board;
   new_random_board; new_random_board; new_random_board; new_random_board;
   new_random_board; new_random_board; new_random_board; new_random_board;
   new_random_board; new_random_board; ]

   (* [garden_tests array] QCheck tests that check for whether the garden is
   incremented correctly and if size stays consistence *) let garden_tests
   (array : A5.Plant.t array array) = (* Check correct increment *) let rows =
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

   (* [random_tests] tests [garden_tests] with random gardens *) let
   random_tests = QCheck2.Test.make ~count:1000 ~name:"random tests" garden_gen
   garden_tests

   (* Converts QCheck tests into OUnit test *) let ounit_random_test =
   QCheck_runner.to_ounit2_test random_tests

   (* OUnit Tests *) let empty_garden = init_garden 2 2

   (* tests to_string *) let test_to_string_empty = "tests to_string" >:: fun _
   -> assert_equal ~printer:(fun x -> x) " " (to_string empty_garden 2 2)

   (* tests [incr_garden]*) let incr_non_empty = incr_garden (pop_non_empty
   (init_garden 2 2) 2 2) 2 2

   (* another test for [incr_garden]*) let test_incr_garden_2 = "tests
   incr_garden" >:: fun _ -> assert_equal ~printer:(fun x -> x) "🌱🌱🌱🌱"
   (to_string incr_non_empty 2 2)

   (* iterates garden [int] times*) let rec iter_garden garden int = if int > 0
   then iter_garden (incr_garden garden 2 2) (int - 1) else garden

   (* garden at oldest age *) let death_garden = iter_garden (pop_non_empty
   (init_garden 2 2) 2 2) 12

   (* checks if garden dies properly *) let test_plant_death = "tests
   incr_garden" >:: fun _ -> assert_equal ~printer:(fun x -> x) " " (to_string
   death_garden 2 2)

   (*all ounit tests*) let suite = "set test suite" >::: [ ounit_random_test;
   test_to_string_empty; test_incr_garden_2; test_plant_death; ]

   let _ = run_test_tt_main suite *)
