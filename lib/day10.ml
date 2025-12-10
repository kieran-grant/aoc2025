open Core

type line_record = {
  indicator_target : string;
  buttons : int list list;
  joltage_requirements : int list;
}

let parse_line_to_record s =
  (* Helper to strip first and last character of a string *)
  let strip_brackets str =
    let len = String.length str in
    if len >= 2 then String.sub str 1 (len - 2) else ""
  in

  (* Split string by space, ignoring empty parts *)
  let parts = String.split_on_char ' ' s |> List.filter (( <> ) "") in

  (* Extract indicator lights from first part *)
  let indicator_target =
    match parts with first :: _ -> strip_brackets first | [] -> ""
  in

  (* Function to parse a (...) group into int list *)
  let parse_button part =
    strip_brackets part |> String.split_on_char ',' |> List.map int_of_string
  in

  (* All parts that are buttons: start from second until we hit a part starting with { *)
  let buttons, joltage_part =
    let rec aux acc = function
      | [] -> (List.rev acc, None)
      | p :: ps ->
          if p.[0] = '{' then (List.rev acc, Some p)
          else aux (parse_button p :: acc) ps
    in
    aux [] (List.tl parts)
  in

  (* Parse joltage requirements *)
  let joltage_requirements =
    match joltage_part with
    | Some jp ->
        strip_brackets jp |> String.split_on_char ',' |> List.map int_of_string
    | None -> []
  in
  { indicator_target; buttons; joltage_requirements }

(*Part 1*)

let toggle c = if c = '.' then '#' else if c = '#' then '.' else c

let flip_char_at_idx s i =
  (* Convert string to bytes (mutable) *)
  let b = Bytes.of_string s in
  (if i < 0 || i >= Bytes.length b then invalid_arg "index out of bounds"
   else
     let current = Bytes.get b i in
     let new_char = toggle current in
     Bytes.set b i new_char);
  (* Convert back to string *)
  Bytes.to_string b

let button_apply_factory (idxs : int list) string =
  List.fold_left
    (fun curr_str curr_idx -> flip_char_at_idx curr_str curr_idx)
    string idxs

let create_initial_lights len = implode (List.init len (fun _ -> '.'))

let create_functions (buttons : int list list) =
  List.map button_apply_factory buttons

module StringSet = Set.Make (String)

(* Each state in the queue: current string, last function index used, path of function indices *)
type bfs_state = {
  current : string;
  last_func : int option; (* None for initial state *)
  path : int list; (* indices of functions applied so far *)
}

let generate_next_states functions (state : bfs_state) (visited : StringSet.t) =
  List.mapi
    (fun idx f ->
      (* Since we have the property that f(f(x)) = x, we don't need to reapply the last function*)
      if Some idx = state.last_func then None
      else
        (*Apply the function to the string*)
        let new_str = f state.current in
        (*If we've seen this string before, exit early, we want to avoid pointless cycles*)
        if StringSet.mem new_str visited then None
        (* Otherwise, our new state is the generated string, the idx of the function we used 
        and the function path so far *)
          else
          Some
            {
              current = new_str;
              last_func = Some idx;
              path = idx :: state.path;
            })
    functions (*Do this over all our functions*)
  (* Turn list of option state into list of state by filtering out None*)
  |> List.filter_map Fun.id

let bfs (line : line_record) =
  let initial = create_initial_lights (String.length line.indicator_target) in
  let target = line.indicator_target in
  let functions = create_functions line.buttons in
  let next_state_generator = generate_next_states functions in

  let rec loop visited queue =
    match queue with
    (* No solution found, we shouldn't get here! *)
    | [] -> None
    | state :: rest ->
        (*We've reached the target, return the list of functions used to get here*)
        if state.current = target then Some (List.rev state.path)
        else
          (* Generate next states *)
          let next_states = next_state_generator state visited in
          let new_visited =
            List.fold_left
              (fun acc st -> StringSet.add st.current acc)
              visited next_states
          in
          loop new_visited (rest @ next_states)
  in
  loop
    (StringSet.singleton initial)
    [ { current = initial; last_func = None; path = [] } ]

let get_shortest_path record =
  match bfs record with
  | None ->
      failwith ("No path possible for target string: " ^ record.indicator_target)
  | Some path -> List.length path

let part_1 raw_text =
  raw_text |> split_lines
  |> List.map parse_line_to_record
  |> List.map get_shortest_path |> sum

(*Part 2*)

let increment_multiple_factory idxs counter =
  let idxs_set = List.sort compare idxs in
  let rec aux i ctr idxs =
    match (ctr, idxs) with
    | [], _ -> []
    | x :: xs, [] -> x :: aux (i + 1) xs [] (* no more indices to increment *)
    | x :: xs, j :: js when i = j -> (x + 1) :: aux (i + 1) xs js
    | x :: xs, _ -> x :: aux (i + 1) xs idxs
  in
  aux 0 counter idxs_set

let create_initial_counter len = List.init len (fun _ -> 0)

let create_inc_functions (buttons : int list list) =
  List.map increment_multiple_factory buttons

(* Each state in the queue: current string, last function index used, path of function indices *)
type pt2_bfs_state = {
  current : int list;
  last_func : int option; (* None for initial state *)
  path : int list; (* indices of functions applied so far *)
}

let all_leq l1 l2 =
  try List.for_all2 (fun x y -> x <= y) l1 l2
  with Invalid_argument _ -> failwith "lists have different lengths"

let generate_next_states_2 functions (target : int list) (state : pt2_bfs_state)
    =
  List.mapi
    (fun idx f ->
      (*Apply the function to the string*)
      let new_counter = f state.current in
      (*If all the parts are less than or equal, then it is a valid state*)
      if all_leq new_counter target then
        Some
          {
            current = new_counter;
            last_func = Some idx;
            path = idx :: state.path;
          }
      else None)
    functions (*Do this over all our functions*)
  (* Turn list of option state into list of state by filtering out None*)
  |> List.filter_map Fun.id

let bfs_2 line =
  let initial =
    create_initial_counter (List.length line.joltage_requirements)
  in
  let target = line.joltage_requirements in
  let functions = create_inc_functions line.buttons in
  let next_state_generator = generate_next_states_2 functions target in
  let visited = Hashtbl.create 100 in

  let rec loop queue =
    match queue with
    | [] -> None
    | state :: rest ->
        if Hashtbl.mem visited state.current then loop rest
        else begin
          Hashtbl.add visited state.current ();
          if state.current = target then Some (List.rev state.path)
          else
            let next_states = next_state_generator state in
            loop (rest @ next_states)
        end
  in
  loop [ { current = initial; last_func = None; path = [] } ]

let get_shortest_path_2 record =
  match bfs_2 record with
  | None ->
      failwith ("No path possible for target string: " ^ record.indicator_target)
  | Some path -> List.length path

(* let part_2 raw_text = *)
(*   raw_text |> split_lines *)
(*   |> List.map parse_line_to_record *)
(*   |> List.map get_shortest_path_2 *)
(*   |> sum *)

let part_2 raw_text =
  let lines = raw_text |> split_lines |> List.map parse_line_to_record in
  let total = List.length lines in
  lines
  |> List.mapi (fun idx record ->
      Printf.printf "Processing line %d/%d\n%!" (idx + 1) total;
      get_shortest_path_2 record)
  |> sum
