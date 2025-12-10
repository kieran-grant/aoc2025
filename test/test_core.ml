open OUnit2
open Aoc2025.Core

(* --- Helpers ---------------------------------------------------------- *)

let string_of_int_list lst =
  "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"

let make_range_test name a b expected =
  name >:: fun _ ->
  assert_equal expected (range a b) ~printer:string_of_int_list

let make_parse_range_test name input expected =
  name >:: fun _ ->
  assert_equal expected (parse_range input) ~printer:string_of_int_list

(* --- Grouped test suites --------------------------------------------- *)

let range_tests =
  "range tests"
  >::: [
         make_range_test "simple increasing range" 1 4 [ 1; 2; 3 ];
         make_range_test "empty when equal" 5 5 [];
         make_range_test "single element" 2 3 [ 2 ];
         make_range_test "larger range" 0 5 [ 0; 1; 2; 3; 4 ];
       ]

let parse_range_tests =
  "parse_range tests"
  >::: [
         make_parse_range_test "basic 1-4" "1-4" [ 1; 2; 3; 4 ];
         make_parse_range_test "5-5 yields empty" "5-5" [ 5 ];
         make_parse_range_test "0-3 example" "0-3" [ 0; 1; 2; 3 ];
         make_parse_range_test "10-13 example" "10-13" [ 10; 11; 12; 13 ];
       ]

let tests = "All tests" >::: [ range_tests; parse_range_tests ]
let _ = run_test_tt_main tests
