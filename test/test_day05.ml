open OUnit2
open Aoc2025.Day05

(* --- Helpers ---------------------------------------------------------- *)

let string_of_int_int (a, b) =
  "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"

let string_of_int_int_list lst =
  "[" ^ String.concat "; " (List.map string_of_int_int lst) ^ "]"

let make_merge_intervals_test name input expected =
  name >:: fun _ ->
  assert_equal expected (merge_intervals input) ~printer:string_of_int_int_list

(* --- Grouped test suites --------------------------------------------- *)

let battery_solver_tests =
  "battery solver tests"
  >::: [
         make_merge_intervals_test "Part 1: Sample 1"
           [ (3, 5); (10, 14); (16, 20); (12, 18) ]
           [ (3, 5); (10, 20) ];
       ]

let tests = "All tests" >::: [ battery_solver_tests ]
let _ = run_test_tt_main tests
