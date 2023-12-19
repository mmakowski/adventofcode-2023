#require "str";;

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

let line_hits line =
  let card = parse_card line in 
  let hits = IntSet.inter card.winning card.got in
  IntSet.cardinal hits
;;

let line_value line =
  hit_value (line_hits line)
;;

let rec process_lines f acc =
  try 
    let line = input_line f
    in process_lines f (acc + line_value line)
  with End_of_file ->
    close_in_noerr f;
    acc
;;

(* let rec dec = function
  | [] -> [];
  | h :: t -> if h = 1 then dec t else (h-1) :: (dec t)
;; *)

let dec l = 
  let dec_drop_zero = function
    | (1, _) -> None;
    | (n ,m) -> Some (n-1, m) in
  List.filter_map dec_drop_zero l
;;

let num_copies dups = 
  let update acc (_, m) = acc + m in
  List.fold_left update 0 dups

let rec process_lines_part_2 f dups acc =
  try 
    let line = input_line f in
    let mul = 1 + num_copies dups in
    let hits = line_hits line in
    let dups_from_hits = if hits = 0 then [] else [(hits, mul)] in
    let next_dups = dups_from_hits @ (dec dups)
    in 
      (* Printf.printf "%d:%d " mul hits;
      List.iter (fun (m,n) -> Printf.printf "(%d,%d)" m n) dups;
      Printf.printf "\n"; *)
      process_lines_part_2 f next_dups (acc + mul)
  with End_of_file ->
    close_in_noerr f;
    acc
;;

let result () = 
  let f = open_in "input-04.txt"
  (* in process_lines f 0 *)
  in process_lines_part_2 f [] 0
;;

Printf.printf "%d\n" (result ());;
