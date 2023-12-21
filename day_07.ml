type hand_type =
  | HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
;;

type hand = {
  typ: hand_type;
  cards: int list
};;

(* comparison is lexicographical by default, so works as intended for `hand` *)

let parse_card ch =
  let str = String.make 1 ch in
  match int_of_string_opt str with
    | Some n -> n;
    | None -> match ch with
      | 'T' -> 10;
      | 'J' -> 1;
      | 'Q' -> 12;
      | 'K' -> 13;
      | 'A' -> 14;
      | _ -> invalid_arg str
;;      

let histogram cards =
  let a = Array.make 15 0 in
  let inc c =
    let incremented = (Array.get a c) + 1 in
    Array.set a c incremented
  in
  List.iter inc cards;
  a
;;

let hand_type cards =
  let hist = histogram cards in
  let num_jokers = Array.get hist 1 in
  Array.set hist 1 0;
  let cmp n m = Int.compare m n in
  Array.sort cmp hist;
  match Array.get hist 0, Array.get hist 1, num_jokers with
    | 5, _, 0 -> FiveOfAKind;
    | 4, _, 1 -> FiveOfAKind;
    | 4, _, 0 -> FourOfAKind;
    | 3, 2, 0 -> FullHouse;
    | 3, _, 2 -> FiveOfAKind;
    | 3, _, 1 -> FourOfAKind;
    | 3, _, 0 -> ThreeOfAKind;
    | 2, 2, 1 -> FullHouse;
    | 2, 2, 0 -> TwoPair;
    | 2, _, 3 -> FiveOfAKind;
    | 2, _, 2 -> FourOfAKind;
    | 2, _, 1 -> ThreeOfAKind;
    | 2, _, 0 -> OnePair;
    | 1, _, 4 -> FiveOfAKind;
    | 1, _, 3 -> FourOfAKind;
    | 1, _, 2 -> ThreeOfAKind;
    | 1, _, 1 -> OnePair;
    | 1, _, 0 -> HighCard;
    | 0, _, 5 -> FiveOfAKind;
    | _, _, _ -> 
      List.iter (Printf.printf "%d ") cards;
      failwith "hand_type: bug!"
;;

let parse_hand str =
  let seq = String.to_seq str in
  let char_list = List.of_seq seq in
  let cards = List.map parse_card char_list in
  let typ = hand_type cards in
  {typ; cards}
;;

type hand_bid = {
  hand: hand;
  bid: int;
};;

let parse_hand_bid str =
  let hand_str::bid_str::[] = String.split_on_char ' ' str in
  let hand = parse_hand hand_str in
  let bid = int_of_string bid_str in
  {hand; bid}
;;

let rec parse_hand_bid_list acc f =
  try
    let line = input_line f in
    let hb = parse_hand_bid line in
    parse_hand_bid_list (hb::acc) f
  with End_of_file ->
    close_in_noerr f;
    List.rev acc
;;

let rec sum acc = function
  | [] -> acc;
  | h::t -> sum (acc+h) t
;;

let result () = 
  let f = open_in "input-07.txt" in
  let hand_bids = parse_hand_bid_list [] f in
  let ordered = List.sort compare hand_bids in
  let scores = List.mapi (fun i hb -> (i+1) * hb.bid) ordered in
  sum 0 scores
;;

Printf.printf "%d\n" (result ());;
