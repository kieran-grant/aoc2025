let sample = {|3-5
10-14
16-20
12-18

1
5
8
11
17
32|}

let () =
  let open Aoc2025.Day05 in
  let open Aoc2025.Core in
  (*Read inputs*)
  let sample_ranges, sample_ids = split_on_blank_line sample in
  let ranges, ids = read_file "inputs/05.txt" |> split_on_blank_line in

  (*Solve*)
  let sample_sol_1 = part_1 sample_ranges sample_ids in
  let sol_1 = part_1 ranges ids in
  let sample_sol_2 = part_2 sample_ranges in
  let sol_2 = part_2 ranges in

  (*Print*)
  Printf.printf "Part 1 (sample): %d\n" sample_sol_1;
  Printf.printf "Part 1: %d\n" sol_1;
  Printf.printf "Part 2 (sample): %d\n" sample_sol_2;
  Printf.printf "Part 2: %d\n" sol_2
