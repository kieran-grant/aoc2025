let sample =
  {|[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}|}

let _ =
  let open Aoc2025.Core in
  let open Aoc2025.Day10 in
  let input = read_file "inputs/10.txt" in
  let sample_sol_1 = part_1 sample in
  let sol_1 = part_1 input in
  let sample_sol_2 = part_2 sample in
  (* let sol_2 = part_2 input in *)

  (*Print solutions*)
  Printf.printf "Part 1 (sample): %d\n" sample_sol_1;
  Printf.printf "Part 1: %d\n" sol_1;
  Printf.printf "Part 2 (sample): %d\n" sample_sol_2
(* Printf.printf "Part 2: %d\n" sol_2 *)
