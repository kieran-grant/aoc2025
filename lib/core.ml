let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Euclidean algorithm for gcd of two ints *)
let rec gcd a b = if b = 0 then abs a else gcd b (a mod b)

(* gcd of a list of ints *)
let gcd_list lst =
  match lst with
  | [] -> invalid_arg "gcd_list: empty list"
  | x :: xs -> List.fold_left gcd x xs

let all_equal = function
  | [] | [ _ ] -> true
  | x :: xs -> List.for_all (( = ) x) xs

let divisors n =
  if n <= 0 then invalid_arg "divisors: n must be positive"
  else
    let rec aux i acc =
      if i * i > n then acc
      else if n mod i = 0 then
        let j = n / i in
        if i = j then aux (i + 1) (i :: acc) (* perfect square *)
        else aux (i + 1) (i :: j :: acc)
      else aux (i + 1) acc
    in
    List.rev (aux 1 [])

let range a b =
  let start = min a b in
  let finish = max a b in
  if finish < start then []
  else List.init (finish - start + 1) (fun i -> start + i)

let rec sum l = match l with [] -> 0 | hd :: tl -> hd + sum tl

(**[explode s] takes in a string and returns it as a list of char*)
let explode s = List.init (String.length s) (String.get s)

(**[implode l] takes in a list of char and returns it as a string*)
let implode l = String.of_seq (List.to_seq l)

let max_of_list lst = List.fold_left max 0 lst
let digit_of_char c = Char.code c - Char.code '0'

let char_of_digit n =
  if n >= 0 && n <= 9 then Char.chr (n + Char.code '0')
  else invalid_arg "digit_of_int: not a single digit"

let foldi f init lst =
  let rec aux i acc = function
    | [] -> acc
    | x :: xs -> aux (i + 1) (f i acc x) xs
  in
  aux 0 init lst

(* Grid map making helpers*)
module PairOrd = struct
  type t = int * int

  let compare (a1, b1) (a2, b2) =
    let c = compare a1 a2 in
    if c <> 0 then c else compare b1 b2
end

module PairMap = Map.Make (PairOrd)

let add_entry map index ch = PairMap.add index ch map
let get_opt_entry map index = PairMap.find_opt index map
let add_idx (a, b) (x, y) = (a + x, b + y)
let keys m = List.map fst (PairMap.bindings m)

let print_pairmap m =
  PairMap.iter
    (fun (row, col) ch -> Printf.printf "(%d, %d) -> %c\n" row col ch)
    m

let create_entries curr_row map entry_string =
  let chars = explode entry_string in
  foldi (fun col acc ch -> add_entry acc (curr_row, col) ch) map chars

let create_map entries =
  foldi
    (fun row acc entry_string -> create_entries row acc entry_string)
    PairMap.empty entries

let replace_many map indices value =
  List.fold_left (fun acc idx -> PairMap.add idx value acc) map indices

(*List printing helper*)
let print_list print_elem lst =
  print_string "[";
  let rec aux = function
    | [] -> ()
    | [ x ] -> print_elem x
    | x :: xs ->
        print_elem x;
        print_string "; ";
        aux xs
  in
  aux lst;
  print_endline "]"

let parse_or_fail str =
  match int_of_string_opt str with
  | Some i -> i
  | None -> failwith (str ^ " is not an integer!")

let parse_hypenated_ints input =
  let items = String.split_on_char '-' input in
  let start = parse_or_fail (List.nth items 0) in
  let stop = parse_or_fail (List.nth items 1) in
  (start, stop)

let parse_range input =
  let start, stop = parse_hypenated_ints input in
  range start (stop + 1)

let split_on_blank_line s =
  let lines = String.split_on_char '\n' s in
  (* Find the blank line *)
  let rec aux before = function
    | [] -> (before, []) (* no blank line found *)
    | "" :: rest -> (before, rest) (* split here *)
    | x :: xs -> aux (before @ [ x ]) xs
  in
  aux [] lines

(* Split input into blocks separated by blank lines *)
let split_blocks (text : string) : string list =
  text
  |> Str.split (Str.regexp "\n[ \t]*\n") (* split on empty lines *)
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")

(* Get the last block *)
let last_block (text : string) : string =
  match List.rev (split_blocks text) with
  | last :: _ -> last
  | [] -> failwith "No blocks found"

(*transpose a list of lists*)
let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss))

(*Flip a list of list horizontally*)
let flip_horizontal xss = List.map List.rev xss

(*Split a string on any whitespace*)
let split_on_whitespace = Str.split (Str.regexp "[ \n\r\x0c\t]+")

(*Returns the last element and the rest of the list*)
let rec last_and_rest = function
  | [] -> failwith "Empty list!"
  | [ x ] -> (x, [])
  | x :: xs ->
      let last, rest = last_and_rest xs in
      (last, x :: rest)

(*Split a string by new lines, removes any empty lines*)
let split_lines raw_txt =
  String.split_on_char '\n' raw_txt |> List.filter (fun x -> x <> "")

let indices_of_char c s =
  String.to_seqi s
  |> Seq.fold_left (fun acc (i, ch) -> if ch = c then i :: acc else acc) []
  |> List.rev

(*Get all unordered unique pairs from a list of elements*)
let rec pairs = function
  | [] -> []
  | x :: xs ->
      (*all pairs where x is paired with each element of xs*)
      let with_x = List.map (fun y -> (x, y)) xs in
      (*plus all pairs that come from the tail*)
      with_x @ pairs xs
