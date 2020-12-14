type result =
  | RoyalFlush
  | StraightFlush of int (* int is the highest card *)
  | FourOfKind of int * int (* first int is the 4, second is the 1 *)
  | FullHouse of int * int (* first int is the 3, second is the 2*)
  | Flush of int * int * int * int * int (* card ranks ordered *)
  | Straight of int (* int is the highest card *)
  | ThreeOfKind of int * int * int (* trips, kicker, kicker *)
  | TwoPair of int * int * int (* pair, pair, kicker *)
  | OnePair of int * int * int * int (* first int is pair,rest are kickers *)
  | HighCard of int (* int is highest card *)

exception Empty

let getRank (card : Deck.card) = card.rank

(** [nth_of_list] returns the nth element of the list [lst]
    Returns an option as the list may not contain that number 
    [lst] is a valid list
    [n] is an int; represents the nth element
    [acc] is an int; the accumulator *)
let rec nth_of_list lst n acc = 
  match lst with
  | [] -> None
  | h :: t -> if acc = n then Some h else nth_of_list t n (acc+1)

(** [n_of_list] returns the first (head) element of the list [lst]
    Returns an option as the list may not contain anything.
    [lst] is a valid list*)
let h_of_list lst =
  nth_of_list lst 0 0

(** [n_of_list] returns the nth element of the list [lst]
    Returns an option as the list may not contain that number.
    [lst] is a valid list
    [n] is an int; represents the nth element *)
let n_of_list lst n =
  nth_of_list lst n 0

let t_of_list lst =
  match lst with 
  | h::t -> t
  | [] -> failwith "Empty list has no tail"

let extract_value = function
  | Some x -> x
  | None -> raise Empty ;;

let create_histogram h = 
  let x : (int * int) list ref = ref [] in
  let add_to_histogram (card : Deck.card) : unit =
    match List.assoc_opt card.rank !x with
    | None -> x := (card.rank, 1) :: !x
    | Some i -> x := (card.rank, i + 1) :: List.remove_assoc card.rank !x in
  List.iter add_to_histogram (Array.to_list h);
  !x 

let compare_cards (c1 : Deck.card) (c2 : Deck.card) = 
  if getRank c1 = getRank c2 then 0
  else if getRank c1 = 1 then -1 
  else if getRank c2 = 1 then 1 
  else if getRank c1 > getRank c2 then -1
  else 1 

let compare_pairs pair1 pair2 = 
  match (pair1, pair2) with
  | ((a,b),(x,y)) -> 
    if b > y then -1 
    else 
      begin if b < y then 1 else (* Needs to be cleaned up / broken up*)
          begin (if a = 1 then -1 
                 else if x = 1 then 1 
                 else if a > x then -1 
                 else if x > a then 1 
                 else 0) end end

(*helper for check_straght and check_straight_flush,
  helps for getting rank with n_of_list
  this is just for documentation purposes, edit this later *)
let get_rank_helper (sorted: Deck.card list) (num: int) = 
  num |> n_of_list sorted |> extract_value |> getRank

let check_straight_helper hd tl snd thd frth = 
  (hd,tl,snd) = (1,2,5) && thd + frth = 7 

let check_straight h = 
  let hsort = List.sort compare_cards (Array.to_list h) in
  let length = List.length hsort in 
  let headrank = getRank (extract_value (h_of_list hsort)) in 
  let app_func = get_rank_helper hsort in 
  let sndrank = app_func 1 in 
  let thdrank = app_func 2 in 
  let frthrank = app_func 3 in 
  let tailrank = app_func (length - 1) in 
  if headrank - tailrank = 4 then Straight (headrank)
  else if check_straight_helper headrank tailrank sndrank thdrank frthrank 
  then Straight 5 
  else if 
    tailrank = 10 && headrank = 1 && frthrank = 11 && sndrank + thdrank = 25 
  then Straight 1
  else HighCard (headrank) 

let check_straight_flush h = 
  let hsort = List.sort compare_cards (Array.to_list h) in
  let length = List.length hsort in 
  let headrank = getRank (extract_value (h_of_list hsort)) in 
  let sndrank = get_rank_helper hsort 1 in 
  let thdrank = get_rank_helper hsort 2 in 
  let frthrank = get_rank_helper hsort 3 in 
  let tailrank = get_rank_helper hsort (length - 1) in 
  if headrank - tailrank = 4 then StraightFlush (headrank)
  else if check_straight_helper headrank tailrank sndrank thdrank frthrank 
  then StraightFlush 5
  else if 
    tailrank = 10 && headrank = 1 && frthrank = 11 && sndrank + thdrank = 25 
  then RoyalFlush
  else 
    let h0 = getRank h.(0) in
    Flush (h0,getRank h.(1),getRank h.(2),getRank h.(3),getRank h.(4)) 


let check_flush h = 
  let sameSuit (suit : char) (card : Deck.card) = card.suit = suit in
  if (Array.for_all (sameSuit 'C') h) then check_straight_flush h
  else if (Array.for_all (sameSuit 'D') h) then check_straight_flush h
  else if (Array.for_all (sameSuit 'H') h) then check_straight_flush h
  else if (Array.for_all (sameSuit 'S') h) then check_straight_flush h
  else check_straight h 

let check41_32_221_2111 x h = 
  match List.sort compare_pairs x with
  | [(x, 4); (y, 1)] -> FourOfKind (x, y)
  | [(x, 3); (y, 2)] -> FullHouse (x, y)
  | [(x, 3); (y, 1); (z, 1)] -> ThreeOfKind (x, y, z)
  | [(x, 2); (y, 2); (z, 1)] -> TwoPair (x, y, z)
  | [(x, 2); (y, 1); (z, 1); (w, 1)] -> OnePair (x,y,z,w)
  | _ -> check_flush h

(** evaluate_hand computes the poker hand given a Deck.card array 
    Returns an instance of type result
    [hand] is a Deck.card array of length 5 *)
let evaluate_hand (hand : Deck.card array) : result =
  let histogram = create_histogram hand in
  check41_32_221_2111 histogram hand

let compare_hands_helper (hand: result) =
  match hand with
  | RoyalFlush -> 10
  | StraightFlush _ -> 9
  | FourOfKind (_,_) -> 8
  | FullHouse (_,_) -> 7
  | Flush (_,_,_,_,_) -> 6
  | Straight _ -> 5
  | ThreeOfKind (_,_,_) -> 4
  | TwoPair (_,_,_) -> 3
  | OnePair (_,_,_,_) -> 2
  | HighCard _ -> 1

(*helper for compare_hands *)
let rec compare_lists x y =
  let first_ele = h_of_list x in 
  let second_ele = h_of_list y in 
  let comp = compare second_ele first_ele in 
  if first_ele = None then 0 
  else if comp <> 0 then comp 
  else compare_lists (t_of_list x) (t_of_list y)

(*informal, clean it up *)
(** [compare_hands] returns an integer based on which one of 2 hands 
    is better in terms of poker rules. If [hand1] has a higher score than [hand2],
    it returns -1. If [hand2] is better than [hand1], it returns 1.
    [hand1] is a valid hand
    [hand2] is a valid hand *)

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
      compare_lists [a1;a2] [b1;b2]
    | (Flush (a1, a2, a3, a4, a5), Flush (b1, b2, b3, b4, b5)) -> 
      compare_lists [a1;a2;a3;a4;a5] [b1;b2;b3;b4;b5]
    | (ThreeOfKind (a1, a2, a3), ThreeOfKind (b1, b2, b3)) 
    | (TwoPair (a1, a2, a3), TwoPair (b1, b2, b3)) -> 
      compare_lists [a1;a2;a3] [b1;b2;b3]
    | (OnePair (a1, a2, a3, a4), OnePair (b1, b2, b3, b4)) -> 
      compare_lists [a1;a2;a3;a4] [b1;b2;b3;b4]
    | _ -> failwith("faulty comparison")

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
    |> List.map evaluate_hand  in 
  match List.sort compare_hands hands with
  | [] -> failwith("empty")
  | h :: _ -> h

let sorted_pairs (p : (Table.person * result) list) =
  let sort_pair p1 p2 =
    compare_hands (snd p1) (snd p2) in
  List.sort sort_pair p


let evaluate_table (table : Table.table)  =
  let rec pairs (list : Table.person list) =
    match list with
    | [] -> []
    | h :: t -> 
      (h, evaluate_hands [|fst (h.hand);snd (h.hand)|] 
         (Array.of_list table.river)) :: pairs t in
  table.in_players 
  |> pairs 
  |> sorted_pairs 
  |> h_of_list 
  |> extract_value 
  |> fst