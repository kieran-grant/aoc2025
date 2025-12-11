let sample =
  {|aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out|}

let _ =
  let open Aoc2025.Core in
  let open Aoc2025.Day11 in
  let input = read_file "inputs/11.txt" in
  let sample_sol_1 = part_1 sample in

  let sol_1 = part_1 input in

  (* let sample_sol_2 = part_2 sample in *)
  (* let sol_2 = part_2 input in *)

  (*Print solutions*)
  Printf.printf "Part 1 (sample): %d\n" sample_sol_1;
  Printf.printf "Part 1: %d\n" sol_1
(* Printf.printf "Part 2 (sample): %d\n" sample_sol_2; *)
(* Printf.printf "Part 2: %d\n" sol_2 *)
