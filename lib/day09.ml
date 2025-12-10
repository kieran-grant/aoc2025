open Core

let rec_size ((x1, y1), (x2, y2)) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

let to_coord lst =
  match lst with
  | [ x; y ] -> (x, y)
  | _ -> failwith "The given list cannot be converted to a 2d point"

let parse_input raw_input =
  split_lines raw_input
  |> List.map (fun s -> String.split_on_char ',' s)
  |> List.map (List.map int_of_string)
  |> List.map to_coord

let max_of_list = List.fold_left max min_int
let get_sizes pairs = List.map (fun x -> (x, rec_size x)) pairs

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

(*Part 2*)

let edge_size ((x1, y1), (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)

let order_by_size size_fun items =
  items
  (*Use size function to get the size*)
  |> List.map (fun item -> (item, size_fun item))
  (*Order descending*)
  |> List.sort (fun (_, s1) (_, s2) -> compare s2 s1)

let zip list_a list_b = List.map2 (fun a b -> (a, b)) list_a list_b

let get_edges vertices =
  zip vertices (List.drop 1 vertices @ [ List.hd vertices ])

let rectangle_fits edges ((x, y), (x', y')) =
  (*Check that no edges intersect the rectangle*)
  let edge_is_outside ((lx, ly), (lx', ly')) =
    (*rightmost point of edge is to the left of leftmost wall of rectangle*)
    max lx lx' <= min x x'
    (*leftmost point of edge is to the right of rightmost wall of rectangle*)
    || min lx lx' >= max x x'
    (*topmost point of edge is below bottommost wall of rectangle*)
    || max ly ly' <= min y y'
    (*bottommost point of edge is above topmost wall of rectangle*)
    || min ly ly' >= max y y'
  in

  List.for_all edge_is_outside edges

let part_2 raw_text =
  let boundry_vertices = parse_input raw_text in

  (*order edges by descending length, bigger edges are more likly to intersect*)
  let edges =
    get_edges boundry_vertices |> order_by_size edge_size |> List.map fst
  in

  (* rectangles ordered by decreasing size, will take the first that fits*)
  let rectangles = boundry_vertices |> pairs |> order_by_size rec_size in

  let rec aux rects =
    match rects with
    | [] -> failwith "Didn't find a rectangle that fits!"
    | (r, size) :: rs -> if rectangle_fits edges r then size else aux rs
  in
  aux rectangles
