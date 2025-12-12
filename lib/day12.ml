open Core

let parse_line (s : string) : int list * int list =
  (* split at ':' *)
  match String.split_on_char ':' s with
  | [ left; right ] ->
      (* left part looks like "47x41" *)
      let dims =
        left |> String.trim |> String.split_on_char 'x'
        |> List.map int_of_string
      in
      (* right part is a list of space-separated integers *)
      let nums =
        right |> String.trim |> String.split_on_char ' '
        |> List.filter (fun x -> x <> "")
        |> List.map int_of_string
      in
      (dims, nums)
  | _ -> failwith ("invalid line: " ^ s)

(* parse multiple lines *)
let parse_lines (lines : string list) : (int list * int list) list =
  List.map parse_line lines

let count_hashes block =
  block |> String.to_seq
  |> Seq.fold_left (fun acc c -> if c = '#' then acc + 1 else acc) 0

(*Get the number of occupied squares for each shape*)
let hash_counts shape_blocks = List.map count_hashes shape_blocks

(* Return (all blocks except last, last block) *)
let split_shapes_and_final text =
  match List.rev (split_blocks text) with
  | last :: rev_rest -> (List.rev rev_rest, last)
  | [] -> failwith "No blocks found"

let can_fit values (operation, coefficients) =
  (*Get the total area*)
  let total_area = List.fold_left ( * ) 1 operation in

  (*Get the total number of occupied squares 
  (sum (num_occuplied_squres_i * instances_of_that_square_in_requirements))*)
  let total_blocks = sum (List.map2 ( * ) coefficients values) in

  (*Make a guess that the blocks pack tightly, 
  so we can just check the number of individual squares fits in the area.
  It doesn't change the answer whether is it <= or < here*)
  total_blocks < total_area

let part_1 raw_text =
  (*Split input into shapes and regions*)
  let shapes, regions = split_shapes_and_final raw_text in
  (*Count number of occupied squares in each block*)
  let num_hashes = hash_counts shapes in
  regions
  (*Split on new line*)
  |> split_lines
  (*Parse regions into ([x_size, y_size]: [number, of, each, block])*)
  |> parse_lines
  (*Get filter out regions where the presents don't fit*)
  |> List.filter (can_fit num_hashes)
  (*Return number of valid regions*)
  |> List.length
