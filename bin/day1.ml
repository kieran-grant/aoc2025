let sample = {|L68
L30
R48
L5
R60
L55
L1
L99
R14
L82|}

let () =
  let open Aoc2025.Day1 in
  (* Prepare inputs *)
  let sample_input = parse_input (String.split_on_char '\n' sample) in
  let input = parse_input (read_lines "inputs/1.txt") in

  (* Compute *)
  let sample_sol_1 = part_1 sample_input in
  let sample_sol_2 = part_2 sample_input in
  let sol_1 = part_1 input in
  let sol_2 = part_2 input in

  (* Print results *)
  Printf.printf "Part 1 (sample): %d\n" sample_sol_1;
  Printf.printf "Part 1: %d\n" sol_1;
  Printf.printf "Part 2 (sample): %d\n" sample_sol_2;
  Printf.printf "Part 2: %d\n" sol_2
