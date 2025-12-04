open OUnit2
open Aoc2025.Day3

(* --- Helpers ---------------------------------------------------------- *)

let string_of_int_list lst =
  "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"

let make_solver_test name n input expected =
  name >:: fun _ ->
  assert_equal expected (solve_for n input) ~printer:string_of_int_list

let make_pipeline_test name n input expected =
  name >:: fun _ ->
  assert_equal expected (solver_pipeline n input) ~printer:string_of_int

(* --- Grouped test suites --------------------------------------------- *)

let battery_solver_tests =
  "battery solver tests"
  >::: [
         make_solver_test "Part 1: Sample 1" 2
           [ 9; 8; 7; 6; 5; 4; 3; 2; 1; 1; 1; 1; 1; 1; 1 ]
           [ 9; 8 ];
         make_solver_test "Part 1: Sample 2" 2
           [ 8; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 9 ]
           [ 8; 9 ];
         make_solver_test "Part 1: Sample 3" 2
           [ 2; 3; 4; 2; 3; 4; 2; 3; 4; 2; 3; 4; 2; 7; 8 ]
           [ 7; 8 ];
         make_solver_test "Part 1: Sample 4" 2
           [ 8; 1; 8; 1; 8; 1; 9; 1; 1; 1; 1; 2; 1; 1; 1 ]
           [ 9; 2 ];
       ]

let part_1_pipeline_tests =
  "part 1 pipeline tests"
  >::: [
         (* Part 1 *)
         make_pipeline_test "Part 1 - Sample 1" 2 "987654321111111" 98;
         make_pipeline_test "Part 1 - Sample 2" 2 "811111111111119" 89;
         make_pipeline_test "Part 1 - Sample 3" 2 "234234234234278" 78;
         make_pipeline_test "Part 1 - Sample 4" 2 "818181911112111" 92;
         (*Part 2*)
         make_pipeline_test "Part 2 - Sample 1" 12 "987654321111111"
           987654321111;
         make_pipeline_test "Part 2 - Sample 2" 12 "811111111111119"
           811111111119;
         make_pipeline_test "Part 2 - Sample 3" 12 "234234234234278"
           434234234278;
         make_pipeline_test "Part 2 - Sample 4" 12 "818181911112111"
           888911112111;
       ]

let tests = "All tests" >::: [ battery_solver_tests; part_1_pipeline_tests ]
let _ = run_test_tt_main tests
