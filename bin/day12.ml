let sample =
  {|0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2|}

let _ =
  let open Aoc2025.Core in
  let open Aoc2025.Day12 in
  let input = read_file "inputs/12.txt" in

  (*
  Note that we get the sample solution wrong, the sample is much harder than the actual input
  *)
  let sample_sol = part_1 sample in
  let sol = part_1 input in

  Printf.printf "Part 1 (sample): %d\n" sample_sol;
  Printf.printf "Part 1: %d\n" sol
