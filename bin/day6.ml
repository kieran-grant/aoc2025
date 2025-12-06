open Aoc2025.Core

let sample = {|123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  |}

let () =
  let open Aoc2025.Day6 in
  (*Read inputs*)
  let input_txt = read_file "inputs/6.txt" in
  let sample_input = parse_input sample in
  let input = parse_input input_txt in

  (* let sample_ranges, sample_ids = split_on_blank_line sample in *)
  (* let ranges, ids = read_file "inputs/5.txt" |> split_on_blank_line in *)
  (* List.iter (print_list print_string) input; *)

  (*Solve*)
  let sample_sol_1 = part_1 sample_input in
  let sol_1 = part_1 input in
  (* let sample_sol_2 = part_2 sample_ranges in *)
  (* let sol_2 = part_2 ranges in *)
  (*Print*)
  Printf.printf "Part 1 (sample): %d\n" sample_sol_1;
  Printf.printf "Part 1: %d\n" sol_1
(* Printf.printf "Part 2 (sample): %d\n" sample_sol_2; *)
(* Printf.printf "Part 2: %d\n" sol_2 *)
