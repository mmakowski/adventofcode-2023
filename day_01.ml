(**
#use "topfind";;
#require "str";;
#use "day_01.ml";;
*)

let line_value line =
  let digit_regexp = Str.regexp "[0-9]" in
  let first_digit_pos = Str.search_forward digit_regexp line 0 in
  let last_digit_pos = Str.search_backward digit_regexp line (String.length line) in
  let digit pos = String.sub line pos 1 in
  let num_str = String.cat (digit first_digit_pos) (digit last_digit_pos) in
  let num = int_of_string num_str in
  (* Printf.printf "%d\n" num; *)
  num
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
  let f = open_in "input-01.txt"
  in process_lines f 0
;;

Printf.printf "%d\n" (result ());;
