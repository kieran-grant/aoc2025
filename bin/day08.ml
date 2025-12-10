let sample =
  {|162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689|}

let _ =
  let open Aoc2025.Day08 in
  let open Aoc2025.Core in
  let input = read_file "inputs/08.txt" in
  let sample_sol_1 = part_1 sample 10 in
  let sol_1 = part_1 input 1000 in
  let sample_sol_2 = part_2 sample in
  let sol_2 = part_2 input in

  (* print_list print_int sample_sol_1 *)
  Printf.printf "Part 1 (sample): %d\n" sample_sol_1;
  Printf.printf "Part 1: %d\n" sol_1;
  Printf.printf "Part 2 (sample): %d\n" sample_sol_2;
  Printf.printf "Part 2: %d\n" sol_2
