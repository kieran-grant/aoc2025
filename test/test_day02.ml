open OUnit2
open Aoc2025.Day02

(* --- Helpers ---------------------------------------------------------- *)

let to_string_from_pair (a, b) = "(" ^ a ^ ", " ^ b ^ ")"

let make_split_test name input expected =
  name >:: fun _ ->
  assert_equal expected (split_string input) ~printer:to_string_from_pair

let string_of_int_list lst =
  "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"

let make_bool_test name f input expected =
  name >:: fun _ -> assert_equal expected (f input) ~printer:string_of_bool

let make_int_list_test name f input expected =
  name >:: fun _ -> assert_equal expected (f input) ~printer:string_of_int_list

let make_string_list_test name f input expected =
  name >:: fun _ ->
  let printer lst =
    "[" ^ String.concat "; " (List.map (fun s -> "\"" ^ s ^ "\"") lst) ^ "]"
  in
  assert_equal expected (f input) ~printer

(* --- Grouped test suites --------------------------------------------- *)

let split_tests =
  "split_string tests"
  >::: [
         make_split_test "splits number with 2 digits" "55" ("5", "5");
         make_split_test "splits number with 6 digits" "123123" ("123", "123");
         make_split_test "splits number with 5 digits" "12312" ("12", "312");
       ]

let count_digits_tests =
  "count_digits tests"
  >::: [
         make_int_list_test "digits of 12341234" count_digits "12341234"
           [ 2; 2; 2; 2 ];
         make_int_list_test "digits of 1111111" count_digits "1111111" [ 7 ];
         make_int_list_test "digits of mixed" count_digits "111211121112"
           [ 9; 3 ];
         make_int_list_test "digits of unique digits" count_digits "012345"
           [ 1; 1; 1; 1; 1; 1 ];
         make_int_list_test
           "digits of unique digits where min doesn't indicate chunk size"
           count_digits "111221112211122" [ 9; 6 ];
       ]

let chunk_string_tests =
  "chunk_string tests"
  >::: [
         make_string_list_test "chunk 12341234 into 2"
           (fun s -> chunk_string s 2)
           "12341234" [ "1234"; "1234" ];
         make_string_list_test "chunk 1212121212 into 5"
           (fun s -> chunk_string s 5)
           "1212121212"
           [ "12"; "12"; "12"; "12"; "12" ];
       ]

let are_all_equal_subsequences_tests =
  "are_all_equal_subsequences tests"
  >::: [
         make_bool_test "123123123 in chunks of 3"
           (fun s -> are_all_equal_subsequences s 3)
           "123123123" true;
         make_bool_test "not equal chunks"
           (fun s -> are_all_equal_subsequences s 2)
           "1221" false;
       ]

let is_invalid_v2_tests =
  "is_invalid_v2 tests (updated)"
  >::: [
         make_bool_test "double repeated pattern 1234 ×2" is_invalid_v2
           "12341234" true;
         make_bool_test "triple repeated pattern 123 ×3" is_invalid_v2
           "123123123" true;
         make_bool_test "fivefold repeated pattern 12 ×5" is_invalid_v2
           "1212121212" true;
         make_bool_test "single digit repeated 1 ×7" is_invalid_v2 "1111111"
           true;
         make_bool_test "complex repeated pattern 1112 ×3" is_invalid_v2
           "111211121112" true;
         make_bool_test "odd length number → valid" is_invalid_v2 "12345" false;
         make_bool_test "single digit odd occurrences 7 ×7 → invalid"
           is_invalid_v2 "7777777" true;
         make_bool_test "multiple digits with odd occurrences → valid"
           is_invalid_v2 "1122335" false;
         make_bool_test "random number, no repeat pattern → valid" is_invalid_v2
           "12345678" false;
       ]

let extra_is_invalid_v2_tests =
  "extra invalid ID tests"
  >::: [
         (*--------------------------------------*)
         (* TRUE INVALIDS your code might miss   *)
         (*--------------------------------------*)
         make_bool_test "triple repeated pattern with zero 010 ×3" is_invalid_v2
           "010010010" true;
         make_bool_test "triple repeated pattern 121 ×3" is_invalid_v2
           "121121121" true;
         make_bool_test "quadruple repeated pattern 9991 ×4" is_invalid_v2
           "9991999199919991" true;
         make_bool_test "quintuple repeated pattern 314 ×5" is_invalid_v2
           "314314314314314" true;
         make_bool_test "quadruple repeated pattern 202 ×4" is_invalid_v2
           "202202202202" true;
         make_bool_test "double repeated pattern with zeros 111222 ×2"
           is_invalid_v2 "111222111222" true;
         make_bool_test "pattern 12121234 ×2 → invalid" is_invalid_v2
           "1212123412121234" true;
         (* Test varying number of divisors of the GCD *)
         make_bool_test
           "single divisor (prime length 7) → invalid if repeated 1111111"
           is_invalid_v2 "1111111" true;
         make_bool_test "GCD with multiple divisors → invalid 1212 ×4"
           is_invalid_v2 "1212121212121212" true;
         make_bool_test "GCD with multiple divisors → invalid 123 ×3"
           is_invalid_v2 "123123123" true;
         make_bool_test "GCD with many divisors, pattern 12 ×6" is_invalid_v2
           "121212121212" true;
         make_bool_test "GCD with many divisors, non-repeated → valid"
           is_invalid_v2 "123412341235" false;
         (*--------------------------------------*)
         (* FALSE INVALIDS your code might think are invalid *)
         (*--------------------------------------*)
         make_bool_test "all digits appear equally but not repeated → valid"
           is_invalid_v2 "121314151617" false;
         make_bool_test "gcd divides length but pattern not repeated → valid"
           is_invalid_v2 "1234512346" false;
         make_bool_test "same multiset in halves but halves differ → valid"
           is_invalid_v2 "112233445566" false;
         make_bool_test "pattern 12121221 ×2 → invalid" is_invalid_v2
           "1212122112121221" true;
         make_bool_test
           "gcd divides length but substring repetition fails → valid"
           is_invalid_v2 "1122334455" false;
         make_bool_test "every digit occurs twice but chunks differ → valid"
           is_invalid_v2 "1029384756" false;
         make_bool_test "all digits count=4, repeated twice → invalid"
           is_invalid_v2 "1234432112344321" true;
         make_bool_test "zero-heavy but not repeated → valid" is_invalid_v2
           "000100020003" false;
       ]

let extra_false_positive_tests =
  "extra false positive tests for is_invalid_v2"
  >::: [
         (* Equal digit counts but no repeated substring *)
         make_bool_test
           "digits 1..4 appear twice but last digit breaks repetition"
           is_invalid_v2 "1234123456" false;
         make_bool_test "digits repeated twice but non-repeating sequence"
           is_invalid_v2 "11223344556677" false;
         make_bool_test "digits 1..4 appear twice in interleaved order"
           is_invalid_v2 "121314151617" false;
         (* GCD > 1 but no repeating substring *)
         make_bool_test "GCD=2, no repeated substring" is_invalid_v2
           "1122334455" false;
         make_bool_test "interleaved digits, GCD>1" is_invalid_v2 "1212123434"
           false;
         make_bool_test "zeros included, GCD>1 but not repeating" is_invalid_v2
           "001122334455" false;
         (* Odd-length numbers cannot repeat fully *)
         make_bool_test "odd length → valid" is_invalid_v2 "12345" false;
         make_bool_test "odd length, some repeated digits" is_invalid_v2 "11121"
           false;
         (* Digit counts suggest repetition but misaligned *)
         make_bool_test "digit counts equal but chunks misaligned" is_invalid_v2
           "12112212" false;
         make_bool_test "another misaligned sequence with GCD=4" is_invalid_v2
           "11212221" false;
         (* Zeros and multiple digits trick GCD *)
         make_bool_test "zeros and repeated digits, non-repeating" is_invalid_v2
           "0101010202" false;
         make_bool_test "multiple digit blocks, counts = 3 each, no repeat"
           is_invalid_v2 "000111222333" false;
         make_bool_test "complex non-repeating pattern" is_invalid_v2
           "112233114422" false;
         make_bool_test "digits in order but interleaved prevents repeat"
           is_invalid_v2 "123123124124" false;
         make_bool_test "counts divisible by 2 but sequence differs"
           is_invalid_v2 "5566778899" false;
       ]
(* --- Master suite ---------------------------------------------------- *)

let tests =
  "All tests"
  >::: [
         split_tests;
         count_digits_tests;
         chunk_string_tests;
         are_all_equal_subsequences_tests;
         is_invalid_v2_tests;
         extra_is_invalid_v2_tests;
         extra_false_positive_tests;
       ]

let _ = run_test_tt_main tests
