open Core

type point3d = float * float * float

let print_point3d (x, y, z) = Printf.printf "(%f, %f, %f)\n" x y z

let print_point3d_pair ((a, b, c), (x, y, z)) =
  Printf.printf "((%f, %f, %f), (%f, %f, %f))\n" a b c x y z

let distance ((a, b, c) : point3d) ((x, y, z) : point3d) =
  sqrt (((a -. x) ** 2.) +. ((b -. y) ** 2.) +. ((c -. z) ** 2.))

let to_point3d lst =
  match lst with
  | [ x; y; z ] -> (x, y, z)
  | _ -> failwith "The given list cannot be converted to a 3d point"

let rec pairs = function
  | [] -> []
  | x :: xs ->
      (* all pairs where x is paired with each element of xs *)
      let with_x = List.map (fun y -> (x, y)) xs in
      (* plus all pairs that come from the tail *)
      with_x @ pairs xs

let parse_input raw_input =
  split_lines raw_input
  |> List.map (String.split_on_char ',')
  |> List.map (List.map float_of_string)
  |> List.map to_point3d

let sort_by_distance pairs =
  let distances = List.map (fun (x, y) -> ((x, y), distance x y)) pairs in
  let sorted = List.sort (fun (_, d1) (_, d2) -> compare d1 d2) distances in
  List.map (fun (a, _) -> a) sorted

let get_pairs_by_distance (lst : point3d list) =
  let all_pairs = pairs lst in
  sort_by_distance all_pairs
