open Core
module StringMap = Map.Make (String)

(* let print_adj_map adj_map = *)
(*   StringMap.iter *)
(*     (fun key neighbors -> *)
(*       Printf.printf "%s -> [" key; *)
(*       List.iter (fun n -> Printf.printf "%s; " n) neighbors; *)
(*       Printf.printf "]\n") *)
(*     adj_map *)

let find_num_paths (start : string) (target : string) adj_map =
  (* print_adj_map adj_map; *)
  let memo : (string, int) Hashtbl.t = Hashtbl.create 32 in

  let rec dfs node =
    if node = target then 1
    else
      match Hashtbl.find_opt memo node with
      | Some n -> n
      | None ->
          let neighbors =
            match StringMap.find_opt node adj_map with
            | Some ls -> ls
            | None -> failwith ("Node " ^ node ^ " could not be found!")
          in
          let total =
            List.fold_left (fun acc neigh -> acc + dfs neigh) 0 neighbors
          in
          Hashtbl.add memo node total;
          total
  in
  dfs start

let parse_line line =
  let parts = String.split_on_char ':' line in
  let key = List.hd parts in
  let rest =
    List.nth parts 1 |> String.split_on_char ' ' |> List.map String.trim
    |> List.filter (fun x -> x <> "")
  in
  (key, rest)

let parse_input raw_txt =
  let lines = split_lines raw_txt |> List.map parse_line in
  List.fold_left (fun m (k, v) -> StringMap.add k v m) StringMap.empty lines

let part_1 raw_text = raw_text |> parse_input |> find_num_paths "you" "out"
