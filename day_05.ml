type entry = {
  src_start: int;
  dest_start: int;
  length: int;
};;

module Entry = struct
  type t = entry

  let compare e1 e2 =
    compare e1.src_start e2.src_start
end;;

type range = {
  start: int;
  length: int;
};;

module Range = struct
  type t = range

  let compare r1 r2 =
    compare r1.start r2.start
end;;

let parse_ranges str =
  let num_strs = String.split_on_char ' ' str in
  let nums = List.filter_map int_of_string_opt num_strs in
  let starts = List.filteri (fun i _ -> i mod 2 = 0) nums in
  let lengths = List.filteri (fun i _ -> i mod 2 = 1) nums in
  let mk_range start length = {start; length} in
  List.map2 mk_range starts lengths
;;

let parse_seeds f =
  let line = input_line f in
  match String.split_on_char ':' line with
    | _ :: seed_nums :: [] -> parse_ranges seed_nums;
    | other -> invalid_arg line
;;

let parse_entry str =
  match String.split_on_char ' ' str with
    | dest :: src :: len :: [] -> {
          src_start = int_of_string src;
          dest_start = int_of_string dest;
          length = int_of_string len;
        };
    | other -> invalid_arg str
;;

let rec fill_gaps start out = function
  | [] -> let last_entry = {
        src_start = start;
        dest_start = start;
        length = Int.max_int - start
      } in List.rev (last_entry::out)
  | h::t -> 
      let next_start = h.src_start + h.length in
      if start < h.src_start then
        let filler = {
          src_start = start;
          dest_start = start;
          length = h.src_start - start
        } in 
        let next_out = h::filler::out in
        fill_gaps next_start next_out t
      else
        fill_gaps next_start (h::out) t
;;

let rec parse_map f map =
  let sorted m = fill_gaps 0 [] (List.sort Entry.compare m) in
  try
    let line = String.trim (input_line f) in
    if String.length line = 0 then
      sorted map, true
    else if String.contains line ':' then
      parse_map f map
    else
      let entry = parse_entry line in
      parse_map f (entry::map)
  with End_of_file -> 
    close_in_noerr f;
    sorted map, false
;;

let rec parse_maps f maps =
  match parse_map f [] with
    | [], _ -> List.rev maps;
    | map, false -> List.rev (map::maps);
    | map, true -> parse_maps f (map::maps)
;;

let parse_input f =
  let seeds = parse_seeds f in
  let _ = input_line f in
  let maps = parse_maps f [] in
  seeds, maps
;;

(** Splits r1 into 1) a range that overlaps r2 and 2) a range that starts right after r2. 
    Assumption: r1.start >= r2.start.
    *)
let split_over r1 r2 =
  if r2.start <= r1.start && r1.start < r2.start + r2.length then
    let over_start = r1.start in
    let over_length = min r1.length (r2.length - (r1.start - r2.start)) in
    let overlap = Some {start = over_start; length = over_length} in
    let remainder =
      if over_length < r1.length then
        let rem_start = r2.start + r2.length in
        let rem_length = r1.length - over_length in
        Some {start = rem_start; length = rem_length}
      else 
        None
    in overlap, remainder 
  else if r1.start >= r2.start + r2.length then
    None, Some r1
  else
    invalid_arg "r1.start < r2.start"
;;

let rec apply_map out src = function
  | [] -> out;
  | {src_start; dest_start; length} :: entries ->
      let map_src_range = {start = src_start; length} in
      let over, rem = split_over src map_src_range in
      match over with
        | None -> apply_map out src entries;
        | Some {start = os; length = ol} ->
            let offset = os - src_start in
            let over_out = {
              start = dest_start + offset;
              length = ol
            } in
            let new_out = over_out::out in
            match rem with
              | None -> new_out;
              | Some new_src -> apply_map new_out new_src entries
;;

let apply_map_ranges ranges map =
  let apply_map_flip = Fun.flip (apply_map []) in
  List.concat_map (apply_map_flip map) ranges
;;

let map_chain maps seeds = 
  List.fold_left apply_map_ranges seeds maps
;;

let result () = 
  let f = open_in "input-05.txt" in
  let seeds, maps = parse_input f in
  let locs = map_chain maps seeds in
  List.fold_left min {start = Int.max_int; length = 0} locs
;;

Printf.printf "%d\n" (result ()).start;;
