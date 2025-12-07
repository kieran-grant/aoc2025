module IntSet = Set.Make (Int)

let indices_of_char c s =
  String.to_seqi s
  |> Seq.fold_left (fun acc (i, ch) -> if ch = c then i :: acc else acc) []
  |> List.rev

let calculate_new_splits xs limit =
  let rec aux xs new_out =
    match xs with
    | [] -> new_out
    | x :: xs -> aux xs (new_out @ [ x - 1; x + 1 ])
  in
  let new_xs = aux (IntSet.to_list xs) [] in
  let filtered = List.filter (fun x -> x >= 0 && x < limit) new_xs in
  IntSet.of_list filtered

let calculate_start strings =
  match strings with
  | [] -> failwith "Emptry string list!"
  | x :: xs -> (String.index x 'S', xs)

let rec main_loop (remaining_rows : string list) (beam_idxs : IntSet.t)
    (num_splits : int) =
  match remaining_rows with
  | [] -> num_splits
  | s :: ss ->
      (*Find the splitters in the row*)
      let splitters = IntSet.of_list (indices_of_char '^' s) in
      (*Get the beams that hit a splitter at this iter*)
      let hits = IntSet.inter splitters beam_idxs in
      (*Add the number of hits to the accumulator*)
      let new_acc = num_splits + IntSet.cardinal hits in
      (*Work out the new beam splits based on the hits*)
      let new_splits = calculate_new_splits hits (String.length s) in
      (*Next set of beam indices will be the (last set \ hits) U new_splits*)
      let new_beam_indices =
        IntSet.union (IntSet.diff beam_idxs hits) new_splits
      in

      (*Recurse*)
      main_loop ss new_beam_indices new_acc

let part_1 strings =
  let initial_beam, rest = calculate_start strings in
  main_loop rest (IntSet.of_list [ initial_beam ]) 0
