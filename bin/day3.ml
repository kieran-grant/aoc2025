let sample = {|987654321111111
811111111111119
234234234234278
818181911112111|}

let () =
  let open Aoc2025.Day3 in
  let open Aoc2025.Core in
  (* Prepare inputs *)
  let sample_input = String.split_on_char '\n' sample in
  let input = read_lines "inputs/3.txt" in

  (* Compute *)
  let sample_sol_1 = part_1 sample_input in

  (* let sample_sol_2 = part_2 sample_input in *)
  let sol_1 = part_1 input in
  (* let sol_2 = part_2 input in *)

  (* Print results *)
  Printf.printf "Part 1 (sample): %d\n" sample_sol_1;
  Printf.printf "Part 1: %d\n" sol_1
(* Printf.printf "Part 2 (sample): %d\n" sample_sol_2; *)
(* Printf.printf "Part 2: %d\n" sol_2 *)
