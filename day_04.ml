(**
#use "topfind";;
#require "str";;
#use "day_03.ml";;
*)

module IntSet = Set.Make(Int);;

type card = {
  winning: IntSet.t;
  got: IntSet.t
};;

let parse_int_set str =
  let num_strs = Str.split (Str.regexp {| +|}) str in
  let num_list = List.map int_of_string num_strs in
  IntSet.of_list num_list
;;

let parse_nums str =
  match String.split_on_char '|' str with
    | winning :: got :: [] -> {
          winning = parse_int_set winning;
          got = parse_int_set got
        };
    | other -> invalid_arg str
;;

let parse_card str =
  match String.split_on_char ':' str with
    | _ :: nums :: [] -> parse_nums nums;
    | other -> invalid_arg str
;;

let rec hit_value = function
  | 0 -> 0;
  | 1 -> 1;
  | n -> 2 * hit_value (n-1)
;;

let line_value line =
  let card = parse_card line in 
  let hits = IntSet.inter card.winning card.got in
  let num_hits = IntSet.cardinal hits in
  hit_value num_hits
;;

let rec process_lines f acc =
  try 
    let line = input_line f
    in process_lines f (acc + line_value line)
  with End_of_file ->
    close_in_noerr f;
    acc
;;

let result () = 
  let f = open_in "input-04.txt"
  in process_lines f 0
;;

Printf.printf "%d\n" (result ());;
