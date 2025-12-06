open Aoc2025.Core

let sample = {|123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  |}

let () =
  let open Aoc2025.Day6 in
  (*Read inputs*)
  let input_txt = read_file "inputs/6.txt" in

  (*Solve*)
  let sample_sol_1 = part_1 sample in
  let sol_1 = part_1 input_txt in
  let sample_sol_2 = part_2 sample in
  let sol_2 = part_2 input_txt in

  (*Print*)
  Printf.printf "Part 1 (sample): %d\n" sample_sol_1;
  Printf.printf "Part 1: %d\n" sol_1;
  Printf.printf "Part 2 (sample): %d\n" sample_sol_2;
  Printf.printf "Part 2: %d\n" sol_2
