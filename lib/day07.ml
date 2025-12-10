open Core
module IntSet = Set.Make (Int)

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

(*Use hash table for memoisation*)
module IntPairHashtbl = struct
  type t = int * int

  let equal (a1, b1) (a2, b2) = a1 = a2 && b1 = b2
  let hash (a, b) = Hashtbl.hash (a, b)
end

module Memo = Hashtbl.Make (IntPairHashtbl)

let indices_of_char c s =
  String.to_seqi s
  |> Seq.fold_left (fun acc (i, ch) -> if ch = c then i :: acc else acc) []
  |> List.rev

let part_2 strings =
  let n_rows = List.length strings in
  let string_array = Array.of_list strings in

  let memo = Memo.create 1000 in

  let rec aux row_idx beam_idx =
    (* reached bottom, add 1 to count of branches*)
    if row_idx >= n_rows then 1
    else
      match Memo.find_opt memo (row_idx, beam_idx) with
      (*If we've already explored this branch, return result*)
      | Some v -> v
      (*Otherwise, we need to work it out for this beam/row pair*)
      | None ->
          let s = string_array.(row_idx) in
          let result =
            match s.[beam_idx] with
            | '^' ->
                (*Calculate new beams*)
                let new_beams =
                  [ beam_idx - 1; beam_idx + 1 ]
                  |> List.filter (fun x -> x >= 0 && x < String.length s)
                in
                (*No new branches created, at the end of the branch*)
                if new_beams = [] then 1
                (*Otherwise, get the sum of the branches that are created*)
                  else
                  List.fold_left
                    (fun acc idx -> acc + aux (row_idx + 1) idx)
                    0 new_beams
            (*If it isn't a beam, continue*)
            | _ -> aux (row_idx + 1) beam_idx
          in
          (*Add to the memoization*)
          Memo.add memo (row_idx, beam_idx) result;

          (*Return result*)
          result
  in

  (* initial beam *)
  let start_idx = String.index string_array.(0) 'S' in
  aux 1 start_idx (* skip first row, since start already counted *)

let part_1 strings =
  let initial_beam, rest = calculate_start strings in
  main_loop rest (IntSet.of_list [ initial_beam ]) 0
