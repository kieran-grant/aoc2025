open OUnit2
open Aoc2025.Day3

(* --- Helpers ---------------------------------------------------------- *)

let make_battery_solver_test name input expected =
  name >:: fun _ ->
  assert_equal expected (solve_battery input) ~printer:string_of_int

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

let tests = "All tests" >::: [ battery_solver_tests ]
let _ = run_test_tt_main tests
