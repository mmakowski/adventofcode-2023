(* #require "str";; *)

type fork = {
  left: string;
  right: string;
};;

module Graph = Map.Make(String);;

let parse_dirs str = 
  str |> String.to_seq |> List.of_seq
;;

let parse_graph_entry str graph =
  let entry_regexp = Str.regexp {|\(...\) = (\(...\), \(...\))|} in
  let matched = Str.string_match entry_regexp str 0 in
  if matched then
    let from = Str.matched_group 1 str in
    let left = Str.matched_group 2 str in
    let right = Str.matched_group 3 str in
    Graph.add from {left; right} graph
  else
    invalid_arg str
;;

let rec parse_graph f acc =
  try
    let line = input_line f in
    let new_acc = parse_graph_entry line acc in
    parse_graph f new_acc
  with End_of_file ->
    close_in_noerr f;
    acc
;;

let parse_input f =
  let dirs_line = input_line f in
  let dirs = parse_dirs dirs_line in
  let _ = input_line f in
  let graph = parse_graph f Graph.empty in
  dirs, graph
;;

let rec walk all_dirs graph step_no loc = function
  | [] -> walk all_dirs graph step_no loc all_dirs;
  | dir::dirs -> 
    if loc = "ZZZ" then
      step_no
    else
      let {left; right} = Graph.find loc graph in
      let next_loc = match dir with
        | 'L' -> left;
        | 'R' -> right;
        | _ -> invalid_arg (String.make 1 dir)
    in walk all_dirs graph (step_no+1) next_loc dirs
;;

let result () = 
  let f = open_in "input-08.txt" in
  let dirs, graph = parse_input f in
  walk dirs graph 0 "AAA" dirs
;;

Printf.printf "%d\n" (result ());;
