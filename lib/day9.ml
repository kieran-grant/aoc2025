open Core

(* Note: 'range' and 'split_lines' are typically from a library (like Core or Batteries).
   Assuming you have a standard library version of these, or have implemented them.
   If not, you may need to define them for the code to compile.
   I will use List.map, List.fold_left, etc. which are standard.
*)

let rec_size ((a, b) : int * int) ((x, y) : int * int) =
  (abs (x - a) + 1) * (abs (y - b) + 1)

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

(*Part 2 helpers*)
module PairOrd = struct
  type t = int * int

  let compare (a1, b1) (a2, b2) =
    let c = compare a1 a2 in
    if c <> 0 then c else compare b1 b2
end

module PairSet = Set.Make (PairOrd)

let gen start finish f =
  range start (finish + 1) |> List.map f |> PairSet.of_list

let points_between (a, b) (x, y) =
  if a = x then gen (min b y) (max b y) (fun v -> (a, v)) (* vertical *)
  else if b = y then gen (min a x) (max a x) (fun v -> (v, b)) (* horizontal *)
  else failwith "row or column does not match"

let generate_boundary_set = function
  | [] | [ _ ] -> failwith "Need at least two vertices"
  | init :: rest ->
      let rec aux vertices acc =
        match vertices with
        | [ x ] -> PairSet.union acc (points_between x init)
        | x :: y :: xs -> aux (y :: xs) (PairSet.union acc (points_between x y))
        | _ -> failwith "Impossible state"
      in
      aux (init :: rest) PairSet.empty

let rectangle_points_list (x1, y1) (x2, y2) =
  (*We only need to check the all the perimeter points as we know the polygon doesn't have any holes*)
  let x_min, x_max = (min x1 x2, max x1 x2) in
  let y_min, y_max = (min y1 y2, max y1 y2) in

  let top_bottom =
    (* Top and bottom edges *)
    List.concat
      [
        List.map (fun x -> (x, y_min)) (range x_min (x_max + 1));
        List.map (fun x -> (x, y_max)) (range x_min (x_max + 1));
      ]
  in

  let sides =
    (* Left and right edges, excluding corners already included *)
    List.concat
      [
        List.map (fun y -> (x_min, y)) (range (y_min + 1) y_max);
        List.map (fun y -> (x_max, y)) (range (y_min + 1) y_max);
      ]
  in

  top_bottom @ sides

let corner_vertices (x1, y1) (x2, y2) =
  let x_min, x_max = (min x1 x2, max x1 x2) in
  let y_min, y_max = (min y1 y2, max y1 y2) in
  [
    (x_min, y_min);
    (* bottom-left *)
    (x_min, y_max);
    (* top-left *)
    (x_max, y_min);
    (* bottom-right *)
    (x_max, y_max) (* top-right *);
  ]

let is_inside boundary_set (x, y) =
  (* Get all boundary points on the same row *)
  let row_points =
    PairSet.elements boundary_set
    |> List.filter (fun (_, by) -> by = y)
    |> List.map fst |> List.sort compare
  in

  (* Count number of segments to the left of x *)
  let rec count_segments lst acc =
    match lst with
    | [] -> acc
    | [ _ ] -> acc
    | a :: b :: rest ->
        if x > a && x <= b then
          (* x is between a and b -> inside *)
          acc + 1
        else count_segments (b :: rest) acc
  in
  let count = count_segments row_points 0 in
  let is_odd = count mod 2 <> 0 in
  is_odd

let is_inside_or_on_boundary boundary_set (x, y) =
  (*check if point lies on the boundry, if not check the number of crossings*)
  PairSet.mem (x, y) boundary_set || is_inside boundary_set (x, y)

let is_rectangle_inside_boundary boundary_set rectangle_points =
  let result =
    List.for_all (is_inside_or_on_boundary boundary_set) rectangle_points
  in

  result

let get_ordered_sizes pairs =
  let ordered =
    pairs
    (*get sizes*)
    |> get_sizes
    (*sort by sizes increasing*)
    |> List.sort (fun (_, d1) (_, d2) -> compare d1 d2)
    (*sort by sizes decreasing*)
    |> List.rev
  in
  ordered

(*TODO: 
I think we can make this more efficient with some memoisation
If we memoise whether each point is inside_or_on_boundry, it avoids doing the inner loop every time
*)
let part_2 raw_text =
  let parsed_input = parse_input raw_text in
  let boundary_set = generate_boundary_set parsed_input in

  (* Compute bounding box *)
  let all_x, all_y = List.split parsed_input in
  let min_x = List.fold_left min max_int all_x in
  let max_x = List.fold_left max min_int all_x in
  let min_y = List.fold_left min max_int all_y in
  let max_y = List.fold_left max min_int all_y in

  (* Lazy memo table *)
  let memo = Hashtbl.create 1024 in

  let is_inside_memo (x, y) =
    match Hashtbl.find_opt memo (x, y) with
    | Some v -> v
    | None ->
        let v =
          if x < min_x || x > max_x || y < min_y || y > max_y then false
          else is_inside_or_on_boundary boundary_set (x, y)
        in
        Hashtbl.add memo (x, y) v;
        v
  in

  let is_rectangle_inside_boundary_memo rectangle_points =
    List.for_all is_inside_memo rectangle_points
  in

  (* Recursive search for largest rectangle that fits *)
  let rec aux pairs_and_sizes =
    match pairs_and_sizes with
    | [] -> failwith "Something went wrong! No points left"
    | ((vertice_a, vertice_b), size) :: ps ->
        let generated_rectangle = rectangle_points_list vertice_a vertice_b in
        if is_rectangle_inside_boundary_memo generated_rectangle then size
        else aux ps
  in

  let ordered_sizes =
    parsed_input |> pairs (* Assumes 'pairs' is defined elsewhere *)
    |> get_ordered_sizes
  in

  aux ordered_sizes

(*TODO: Better (?) implementation for part 2


Generate the rectangle:
- Check if any vertices (from the initial set) inside of it, if so then we know it isn't a candidate

*)

let point_strictly_inside_rectangle (x1, y1) (x2, y2) (px, py) =
  let x_min, x_max = (min x1 x2, max x1 x2) in
  let y_min, y_max = (min y1 y2, max y1 y2) in
  px > x_min && px < x_max && py > y_min && py < y_max

let any_vertices_in_rectangle boundry_vertices v1 v2 =
  List.exists
    (fun v -> point_strictly_inside_rectangle v1 v2 v)
    boundry_vertices

let is_inside_or_on_boundary boundary_set (x, y) =
  (*check if point lies on the boundry, if not check the number of crossings*)
  PairSet.mem (x, y) boundary_set || is_inside boundary_set (x, y)

let part_2' raw_text =
  let boundry_vertices = parse_input raw_text in
  let boundary_set = generate_boundary_set boundry_vertices in

  let memo = Hashtbl.create 128 in

  let is_inside_or_on_boundary_memo (x, y) =
    match Hashtbl.find_opt memo (x, y) with
    | Some v -> v
    | None ->
        let v = is_inside_or_on_boundary boundary_set (x, y) in
        Hashtbl.add memo (x, y) v;
        v
  in
  let is_rectangle_inside_boundary_memo rectangle_points =
    List.for_all is_inside_or_on_boundary_memo rectangle_points
  in
  (* Recursive search for largest rectangle that fits *)
  let rec aux pairs_and_sizes =
    match pairs_and_sizes with
    | [] -> failwith "Something went wrong! No points left"
    | ((vertice_a, vertice_b), size) :: ps ->
        let corners = corner_vertices vertice_a vertice_b in
        (*Check 4 vertices are in the boundry at least*)
        if is_rectangle_inside_boundary_memo corners then
          (*Then need to check the rest of the perimeter*)
          let generated_rectangle = rectangle_points_list vertice_a vertice_b in
          if is_rectangle_inside_boundary_memo generated_rectangle then size
          else aux ps
        else aux ps
  in

  let ordered_sizes =
    boundry_vertices |> pairs (* Assumes 'pairs' is defined elsewhere *)
    |> get_ordered_sizes
  in

  aux ordered_sizes
