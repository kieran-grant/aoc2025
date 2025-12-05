open Core

let split_string string =
  let len = String.length string in
  let mid = len / 2 in
  let first = String.sub string 0 mid in
  let second = String.sub string mid (len - mid) in
  (first, second)

let is_invalid num_string =
  String.length num_string mod 2 = 0
  &&
  let f, s = split_string num_string in
  f = s

let count_digits s =
  let counts = Array.make 10 0 in
  String.iter
    (fun c ->
      if '0' <= c && c <= '9' then
        counts.(Char.code c - Char.code '0') <-
          counts.(Char.code c - Char.code '0') + 1)
    s;
  List.filter (fun x -> x > 0) (Array.to_list counts)

let chunk_string s k =
  let n = String.length s in
  let chunk_size = n / k in
  List.init k (fun i -> String.sub s (i * chunk_size) chunk_size)

let are_all_equal_subsequences num_string len =
  let chunks = chunk_string num_string len in
  all_equal chunks

let try_divisors num_string greatest_common_divisor =
  let divisors_of_gcd =
    List.filter (( < ) 1) (divisors greatest_common_divisor)
  in
  let rec aux dvs =
    match dvs with
    | [] -> false (* return false if no divisor worked *)
    | x :: xs ->
        if are_all_equal_subsequences num_string x then true else aux xs
  in
  aux divisors_of_gcd

let is_invalid_v2 num_string =
  let occurances = count_digits num_string in
  if List.length occurances = 1 then List.nth occurances 0 >= 2
  else
    let common_divisor = gcd_list occurances in
    if common_divisor = 1 then false else try_divisors num_string common_divisor

let parse_input input = String.split_on_char ',' input

let part_maker is_invalid_fn input =
  (*Split string by comma*)
  parse_input input
  (*Change n-m to a list [n, n+1, ..., m-1, m] *)
  |> List.map parse_range
  (*Flatten list of lists into a list of ints*)
  |> List.flatten
  (*Convert ints to strings*)
  |> List.map string_of_int
  (*Get the invalid items*)
  |> List.filter is_invalid_fn
  (*Convert back to int*)
  |> List.map int_of_string
  (*Get the total*)
  |> sum

let part_1 = part_maker is_invalid
let part_2 = part_maker is_invalid_v2
