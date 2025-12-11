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

let find_paths (start : string) (target : string) adj_map =
  (* print_adj_map adj_map; *)
  let memo : (string, string list list) Hashtbl.t = Hashtbl.create 32 in

  let rec dfs node =
    if node = target then [ [ target ] ]
    else
      match Hashtbl.find_opt memo node with
      | Some lst -> lst
      | None ->
          let neighbors =
            match StringMap.find_opt node adj_map with
            | Some ls -> ls
            | None -> failwith ("Node " ^ node ^ " could not be found!")
          in
          (* For each neighbor, get all paths and prepend current node *)
          let paths =
            List.concat_map
              (fun neigh -> List.map (fun path -> node :: path) (dfs neigh))
              neighbors
          in
          Hashtbl.add memo node paths;
          paths
  in
  dfs start

(* (* Print a single path as "A -> B -> C" *) *)
(* let print_path path = *)
(*   match path with *)
(*   | [] -> () *)
(*   | hd :: tl -> *)
(*       print_string hd; *)
(*       List.iter (fun node -> print_string (" -> " ^ node)) tl; *)
(*       print_newline () *)

(* Print a list of paths *)
(* let print_paths paths = List.iter print_path paths *)

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

let get_valid_paths (paths : string list list) =
  List.filter
    (fun path ->
      List.exists (fun x -> x = "fft") path
      && List.exists (fun x -> x = "dac") path)
    paths

let part_1 raw_text = raw_text |> parse_input |> find_num_paths "you" "out"

let part_2 raw_text =
  raw_text |> parse_input |> find_paths "svr" "out" |> get_valid_paths
  |> List.length
