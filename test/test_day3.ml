open OUnit2
open Aoc2025.Day3

(* --- Helpers ---------------------------------------------------------- *)

let make_battery_solver_test name input expected =
  name >:: fun _ ->
  assert_equal expected (solve_battery input) ~printer:string_of_int

let make_pipeline_test name input expected =
  name >:: fun _ ->
  assert_equal expected (part_1_pipeline input) ~printer:string_of_int

(* --- Grouped test suites --------------------------------------------- *)

let battery_solver_tests =
  "battery solver tests"
  >::: [
         make_battery_solver_test "Sample 1"
           [ 9; 8; 7; 6; 5; 4; 3; 2; 1; 1; 1; 1; 1; 1; 1 ]
           98;
         make_battery_solver_test "Sample 2"
           [ 8; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 9 ]
           89;
         make_battery_solver_test "Sample 3"
           [ 2; 3; 4; 2; 3; 4; 2; 3; 4; 2; 3; 4; 2; 7; 8 ]
           78;
         make_battery_solver_test "Sample 4"
           [ 8; 1; 8; 1; 8; 1; 9; 1; 1; 1; 1; 2; 1; 1; 1 ]
           92;
       ]

let part_1_pipeline_tests =
  "part 1 pipeline tests"
  >::: [
         make_pipeline_test "Sample 1" "987654321111111" 98;
         make_pipeline_test "Sample 2" "811111111111119" 89;
         make_pipeline_test "Sample 3" "234234234234278" 78;
         make_pipeline_test "Sample 4" "818181911112111" 92;
       ]

let tests = "All tests" >::: [ battery_solver_tests; part_1_pipeline_tests ]
let _ = run_test_tt_main tests
