let sample =
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

let () =
  let open Aoc2025.Day02 in
  let open Aoc2025.Core in
  (* Prepare inputs *)
  let input = List.nth (read_lines "inputs/02.txt") 0 in

  (* Compute *)
  let sample_sol_1 = part_1 sample in
  let sample_sol_2 = part_2 sample in
  let sol_1 = part_1 input in
  let sol_2 = part_2 input in
  (* Print results *)
  Printf.printf "Part 1 (sample): %d\n" sample_sol_1;
  Printf.printf "Part 1: %d\n" sol_1;
  Printf.printf "Part 2 (sample): %d\n" sample_sol_2;
  Printf.printf "Part 2: %d\n" sol_2
