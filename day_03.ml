#require "str";;

type point = {
  x: int;
  y: int
}

module Point = struct
  type t = point

  let compare p1 p2 =
    let cmp_x = compare p1.x p2.x in
    if cmp_x <> 0 then
      cmp_x
    else
      compare p1.y p2.y
end;;

type number = {
  value: int;
  top_left: point;
  width: int;
};;

module PointSet = Set.Make(Point);;

type schematic = {
  numbers: number list;
  parts: PointSet.t
};;

let rec parse_numbers line y start_x acc =
  let num_regexp = Str.regexp {|[0-9]+|} in
  try
    let num_x = Str.search_forward num_regexp line start_x in
    let value_str = Str.matched_string line in
    let value_width = String.length value_str in
    let num = {
      value = int_of_string value_str; 
      top_left = {x = num_x-1; y = y-1}; 
      width = value_width + 2
    } in
    parse_numbers line y (num_x+value_width) (num::acc)
  with Not_found -> List.rev acc
;;

let rec parse_parts line y start_x acc =
  let part_regexp = Str.regexp {|[^0-9.]|} in
  try
    let part_x = Str.search_forward part_regexp line start_x in
    let part = {x = part_x; y = y} in
    parse_parts line y (part_x+1) (PointSet.add part acc)
  with Not_found -> acc
;;

let update_schematic schematic line y =
  let line_numbers = parse_numbers line y 0 [] in
  let line_parts = parse_parts line y 0 PointSet.empty in
  {
    numbers = schematic.numbers @ line_numbers;
    parts = PointSet.union schematic.parts line_parts
  }
;;

let rec process_lines f schematic y =
  try 
    let line = input_line f
    in process_lines f (update_schematic schematic line y) (y + 1)
  with End_of_file ->
    close_in_noerr f;
    schematic
;;

let parse_schematic f =
  process_lines f {numbers = []; parts = PointSet.empty} 0
;;

let is_part_num parts num =
  let line y i = {x = num.top_left.x + i; y} in
  let coords = List.init num.width (line num.top_left.y) @ 
               [{x = num.top_left.x; y = num.top_left.y + 1};
                {x = num.top_left.x + num.width - 1; y = num.top_left.y + 1}
               ] @
               List.init num.width (line (num.top_left.y + 2))
  in List.exists (fun p -> PointSet.mem p parts) coords
;;

let part_num_values schematic =
  let part_nums = List.filter (is_part_num schematic.parts) schematic.numbers in
  let value {value = v; _} = v in
  List.map value part_nums
;;

let rec sum acc = function
  | [] -> acc
  | h::t -> sum (acc+h) t
;;

let result () = 
  let f = open_in "input-03.txt" in
  let schematic = parse_schematic f in
  sum 0 (part_num_values schematic)
;;

Printf.printf "%d\n" (result ());;
