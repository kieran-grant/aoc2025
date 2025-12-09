open Core

let rec_size ((a, b) : int * int) ((x, y) : int * int) =
  (abs (x - a) + 1) * (abs (y - b) + 1)

let to_coord lst =
  match lst with
  | [ x; y ] -> (x, y)
  | _ -> failwith "The given list cannot be converted to a 3d point"

let parse_input raw_input =
  split_lines raw_input
  |> List.map (String.split_on_char ',')
  |> List.map (List.map int_of_string)
  |> List.map to_coord

let max_of_list = List.fold_left max min_int
let get_sizes pairs = List.map (fun (x, y) -> ((x, y), rec_size x y)) pairs

let get_max_size pairs_and_sizes =
  pairs_and_sizes
  (*Pull out the size*)
  |> List.map (fun (_, d) -> d)
  (*Get the max element*)
  |> max_of_list

let part_1 raw_input =
  raw_input
  (*Parse input into list of coords*)
  |> parse_input
  (*Generate list of all unordered pairs of coords*)
  |> pairs
  (*Get sizes of each rectangle*)
  |> get_sizes
  (*Get the biggest size*)
  |> get_max_size
