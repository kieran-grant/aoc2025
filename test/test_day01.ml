open OUnit2
open Aoc2025.Day01

let to_string_from_pair pair =
  "(" ^ string_of_int (fst pair) ^ ", " ^ string_of_int (snd pair) ^ ")"

let make_click_test name expected_output current_state rotation =
  name >:: fun _ ->
  assert_equal expected_output
    (calculate_clicks current_state rotation)
    ~printer:to_string_from_pair

let tests =
  "test suite for zero-click counting"
  >::: [
         (* From the AoC example sequence: *)
         make_click_test "L68 from 50 → ends at 82, hits 0 once during rotation"
           (82, 1) 50
           { direction = 'L'; amount = 68 };
         make_click_test "L30 from 82 → ends at 52, hits 0 zero times" (52, 0)
           82
           { direction = 'L'; amount = 30 };
         make_click_test "R48 from 52 → ends at 0, hits 0 at end" (0, 1) 52
           { direction = 'R'; amount = 48 };
         make_click_test "L5 from 0 → ends at 95, zero hits" (95, 0) 0
           { direction = 'L'; amount = 5 };
         make_click_test "R60 from 95 → ends at 55, hits 0 once during rotation"
           (55, 1) 95
           { direction = 'R'; amount = 60 };
         make_click_test "L55 from 55 → ends at 0, hits 0 at end" (0, 1) 55
           { direction = 'L'; amount = 55 };
         make_click_test "L1 from 0 → ends at 99, zero hits" (99, 0) 0
           { direction = 'L'; amount = 1 };
         make_click_test "L99 from 99 → ends at 0, hits 0 at end" (0, 1) 99
           { direction = 'L'; amount = 99 };
         make_click_test "R14 from 0 → ends at 14, zero hits" (14, 0) 0
           { direction = 'R'; amount = 14 };
         make_click_test "L82 from 14 → ends at 32, hits 0 once during rotation"
           (32, 1) 14
           { direction = 'L'; amount = 82 };
         (* Optional sanity check: big rotations *)
         make_click_test "R1000 from 50 → returns to 50, hits 0 ten times"
           (50, 10) 50
           { direction = 'R'; amount = 1000 };
       ]

let _ = run_test_tt_main tests
