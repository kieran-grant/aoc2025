let sample = {|7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3|}

let _ =
  let open Aoc2025.Day9 in
  let open Aoc2025.Core in
  let input = read_file "inputs/9.txt" in
  let sample_sol_1 = part_1 sample in
  let sol_1 = part_1 input in
  let sample_sol_2 = part_2' sample in
  let sol_2 = part_2' input in

  Printf.printf "Part 1 (sample): %d\n" sample_sol_1;
  Printf.printf "Part 1: %d\n" sol_1;
  Printf.printf "Part 2 (sample): %d\n" sample_sol_2;
  Printf.printf "Part 2: %d\n" sol_2
