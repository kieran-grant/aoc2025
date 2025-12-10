let sample =
  {|..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.|}

let () =
  let open Aoc2025.Day04 in
  let open Aoc2025.Core in
  let sample_input = String.split_on_char '\n' sample in
  let input = read_lines "inputs/04.txt" in

  let sample_sol_1 = part_1 sample_input in
  let sol_1 = part_1 input in
  let sample_sol_2 = part_2 sample_input in
  let sol_2 = part_2 input in

  Printf.printf "Part 1 (sample): %d\n" sample_sol_1;
  Printf.printf "Part 1: %d\n" sol_1;
  Printf.printf "Part 2 (sample): %d\n" sample_sol_2;
  Printf.printf "Part 2: %d\n" sol_2
