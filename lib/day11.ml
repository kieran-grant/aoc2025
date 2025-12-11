open Core
module StringMap = Map.Make (String)

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

let find_num_paths_2 (start : string) (target : string) adj_map =
  (* Memo table key includes all state that affects the result *)
  let memo : (string * bool * bool, int) Hashtbl.t = Hashtbl.create 64 in

  let rec dfs node seen_dac seen_fft =
    (* If we hit the target AND have seen both required nodes, it's a valid path *)
    if node = target then if seen_dac && seen_fft then 1 else 0
    else
      let key = (node, seen_dac, seen_fft) in
      match Hashtbl.find_opt memo key with
      | Some n -> n
      | None ->
          let new_seen_dac = seen_dac || node = "dac" in
          let new_seen_fft = seen_fft || node = "fft" in

          let neighbors =
            match StringMap.find_opt node adj_map with
            | Some ls -> ls
            | None -> failwith ("Node " ^ node ^ " could not be found!")
          in

          let total =
            List.fold_left
              (fun acc neigh -> acc + dfs neigh new_seen_dac new_seen_fft)
              0 neighbors
          in

          Hashtbl.add memo key total;
          total
  in

  dfs start false false

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
let part_2 raw_text = raw_text |> parse_input |> find_num_paths_2 "svr" "out"
