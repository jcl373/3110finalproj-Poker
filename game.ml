type result =
  | RoyalFlush
  | StraightFlush of int (* int is the highest card *)
  | FourOfKind of int * int (* first int is the 4, second is the 1 *)
  | FullHouse of int * int (* first int is the 3, second is the 2*)
  | Flush of int * int * int * int * int (* card ranks ordered *)
  | Straight of int (* int is the highest card *)
  | ThreeOfKind of int * int * int (* trips, kicker, kicker *)
  | TwoPair of int * int * int (* pair, pair, kicker *)
  | OnePair of int * int * int * int (* first int is pair, rest are kickers *)
  | HighCard of int (* int is highest card *)

exception Empty

let getRank (card : Deck.card) = card.rank

let t_of_list lst = 
  match lst with 
  | h :: t -> t
  | [] -> failwith "Empty list has no tail"

let create_histogram hand = 
  let hist : (int * int) list ref = ref [] in
  let add_to_histogram (card : Deck.card) : unit =
    match List.assoc_opt card.rank !hist with
    | None -> hist := (card.rank, 1) :: !hist
    | Some i -> hist := (card.rank, i + 1) :: List.remove_assoc card.rank !hist 
  in
  List.iter add_to_histogram (Array.to_list hand);
  !hist

let compare_cards (card1 : Deck.card) (card2 : Deck.card) = 
  if getRank card1 = getRank card2 then 0
  else if getRank card1 = 1 then -1 
  else if getRank card2 = 1 then 1 
  else if getRank card1 > getRank card2 then -1
  else 1 

let compare_pairs pair1 pair2 = 
  let compare_first first second =
    if first = 1 then -1 
    else if second = 1 then 1 
    else if first > second then -1 
    else if second > first then 1 
    else 0 in
  match (pair1, pair2) with
  | ((a, b), (x, y)) -> 
    if b > y then -1 
    else if b < y then 1 
    else compare_first a x

(* helper for check_straght and check_straight_flush,
   helps for getting rank with n_of_list *)
let get_rank_helper (sorted : Deck.card list) (num : int) = 
  num |> List.nth_opt sorted |> Option.get |> getRank

let check_straight_helper hd tl snd thd frth = 
  (hd, tl, snd) = (1, 2, 5) && thd + frth = 7 

let check_straight hand = 
  let hsort = List.sort compare_cards (Array.to_list hand) in
  let length = List.length hsort in 
  let headrank = getRank (Option.get (List.nth_opt hsort 0)) in 
  let app_func = get_rank_helper hsort in 
  let sndrank = app_func 1 in 
  let thdrank = app_func 2 in 
  let frthrank = app_func 3 in 
  let tailrank = app_func (length - 1) in

  if headrank - tailrank = 4 then Straight headrank
  else if check_straight_helper headrank tailrank sndrank thdrank frthrank 
  then Straight 5 
  else if tailrank = 10 && headrank = 1 && frthrank = 11 && 
          sndrank + thdrank = 25 
  then Straight 1
  else HighCard headrank

let check_straight_flush hand = 
  let hsort = List.sort compare_cards (Array.to_list hand) in
  let length = List.length hsort in 
  let headrank = getRank (Option.get (List.nth_opt hsort 0)) in 
  let sndrank = get_rank_helper hsort 1 in 
  let thdrank = get_rank_helper hsort 2 in 
  let frthrank = get_rank_helper hsort 3 in 
  let tailrank = get_rank_helper hsort (length - 1) in 

  if headrank - tailrank = 4 then StraightFlush headrank
  else if check_straight_helper headrank tailrank sndrank thdrank frthrank 
  then StraightFlush 5
  else if tailrank = 10 && headrank = 1 && frthrank = 11 && 
          sndrank + thdrank = 25 
  then RoyalFlush
  else 
    let h0 = getRank hand.(0) in
    Flush (h0, getRank hand.(1), getRank hand.(2), getRank hand.(3), 
           getRank hand.(4)) 

let check_flush hand = 
  let same_suit (suit : char) (card : Deck.card) = card.suit = suit in
  if (Array.for_all (same_suit 'C') hand) then check_straight_flush hand
  else if (Array.for_all (same_suit 'D') hand) then check_straight_flush hand
  else if (Array.for_all (same_suit 'H') hand) then check_straight_flush hand
  else if (Array.for_all (same_suit 'S') hand) then check_straight_flush hand
  else check_straight hand 

let check41_32_221_2111 pair hand = 
  match List.sort compare_pairs pair with
  | [(x, 4); (y, 1)] -> FourOfKind (x, y)
  | [(x, 3); (y, 2)] -> FullHouse (x, y)
  | [(x, 3); (y, 1); (z, 1)] -> ThreeOfKind (x, y, z)
  | [(x, 2); (y, 2); (z, 1)] -> TwoPair (x, y, z)
  | [(x, 2); (y, 1); (z, 1); (w, 1)] -> OnePair (x, y, z, w)
  | _ -> check_flush hand

let evaluate_hand (hand : Deck.card array) : result =
  let histogram = create_histogram hand in
  check41_32_221_2111 histogram hand

let compare_hands_helper (hand : result) =
  match hand with
  | RoyalFlush -> 10
  | StraightFlush _ -> 9
  | FourOfKind (_, _) -> 8
  | FullHouse (_, _) -> 7
  | Flush (_, _, _, _, _) -> 6
  | Straight _ -> 5
  | ThreeOfKind (_, _, _) -> 4
  | TwoPair (_, _, _) -> 3
  | OnePair (_, _, _, _) -> 2
  | HighCard _ -> 1

(* helper for compare_hands *)
let rec compare_lists lst1 lst2 =
  let first_ele = List.nth_opt lst1 0 in 
  let second_ele = List.nth_opt lst2 0 in 
  let comp = compare second_ele first_ele in 

  if first_ele = None then 0 
  else if comp <> 0 then comp 
  else compare_lists (t_of_list lst1) (t_of_list lst2)

let compare_hands (hand1 : result) (hand2 : result) : int =
  let h1 = compare_hands_helper hand1 in
  let h2 = compare_hands_helper hand2 in 

  if h1 > h2 then -1
  else if h2 > h1 then 1
  else match (hand1, hand2) with
    | (RoyalFlush, RoyalFlush) -> 0
    | (StraightFlush a, StraightFlush b) | (HighCard a, HighCard b)
    | (Straight a, Straight b) -> compare_lists [a] [b]
    | (FourOfKind (a1, a2), FourOfKind (b1, b2)) 
    | (FullHouse (a1, a2), FullHouse (b1, b2))-> 
      compare_lists [a1; a2] [b1; b2]
    | (Flush (a1, a2, a3, a4, a5), Flush (b1, b2, b3, b4, b5)) -> 
      compare_lists [a1; a2; a3; a4; a5] [b1; b2; b3; b4; b5]
    | (ThreeOfKind (a1, a2, a3), ThreeOfKind (b1, b2, b3)) 
    | (TwoPair (a1, a2, a3), TwoPair (b1, b2, b3)) -> 
      compare_lists [a1; a2; a3] [b1; b2; b3]
    | (OnePair (a1, a2, a3, a4), OnePair (b1, b2, b3, b4)) -> 
      compare_lists [a1; a2; a3; a4] [b1; b2; b3; b4]
    | _ -> failwith("Faulty comparison")

(* n choose k mathematical operation *)
let rec choose n k =
  if n <= 0 then [ [] ]
  else match k with
    | [] -> []
    | h :: t -> 
      let before = List.map (fun x -> h :: x) (choose (n-1) t) in
      let after = choose n t in
      before @ after

let evaluate_hands (hole : Deck.card array) (community : Deck.card array) =
  let get_deck = 
    community 
    |> Array.append hole 
    |> Array.to_list 
    |> choose 5 in 
  let hands =  
    get_deck 
    |> List.map Array.of_list 
    |> List.map evaluate_hand in 
  match List.sort compare_hands hands with
  | [] -> failwith("No hands")
  | h :: _ -> h

let sorted_pairs (pairs : (Table.person * result) list) =
  let sort_pair p1 p2 = compare_hands (snd p1) (snd p2) in
  List.sort sort_pair pairs

let evaluate_table (table : Table.table)  =
  let rec pairs (list : Table.person list) =
    match list with
    | [] -> []
    | h :: t -> (h, evaluate_hands [|fst (h.hand);snd (h.hand)|] 
                   (Array.of_list table.river)) :: pairs t 
  in
  List.nth_opt (sorted_pairs (pairs (table.in_players))) 0
  |> Option.get 
  |> fst