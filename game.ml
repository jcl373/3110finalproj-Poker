type result =
  | RoyalFlush
  | StraightFlush of int (* int is the highest card *)
  | FourOfKind of int * int (* first int is the 4, second is the 1 *)
  | FullHouse of int * int (* first int is the 3, second is the 2*)
  | Flush of int * int * int * int * int (* card ranks ordered *)
  | Straight of int (* int is the highest card *)
  | ThreeOfKind of int * int * int (* trips, kicker, kicker *)
  | TwoPair of int * int * int (* pair, pair, kicker *)
  | OnePair of int * int * int * int (* first int is the pair, rest are kickers *)
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

let check_straight h = 
  let hsort = (List.sort compare_cards (Array.to_list h)) in
  let length = List.length hsort in 
  let headrank = getRank (extract_value (h_of_list hsort)) in 
  let sndrank = get_rank_helper hsort 1 in 
  let thdrank = get_rank_helper hsort 2 in 
  let frthrank = get_rank_helper hsort 3 in 
  let tailrank = get_rank_helper hsort (length - 1) in 
  if headrank - tailrank = 4 then Straight (headrank)
  else if headrank = 1 && tailrank = 2 && sndrank = 5 && thdrank + frthrank = 7 then Straight 5
  else if tailrank = 10 && headrank = 1 && frthrank = 11 && sndrank + thdrank = 25 then Straight 1
  else HighCard (headrank) 

let check_straight_flush h = 
  let hsort = (List.sort compare_cards (Array.to_list h)) in
  let length = List.length hsort in 
  let headrank = getRank (extract_value (h_of_list hsort)) in 
  let sndrank = get_rank_helper hsort 1 in 
  let thdrank = get_rank_helper hsort 2 in 
  let frthrank = get_rank_helper hsort 3 in 
  let tailrank = get_rank_helper hsort (length - 1) in 
  if headrank - tailrank = 4 then StraightFlush (headrank)
  else if headrank = 1 && tailrank = 2 && sndrank = 5 && thdrank + frthrank = 7 then StraightFlush 5
  else if tailrank = 10 && headrank = 1 && frthrank = 11 && sndrank + thdrank = 25 then RoyalFlush
  else Flush (getRank h.(0),getRank h.(1),getRank h.(2),getRank h.(3),getRank h.(4)) 

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

let evaluate_hand (h : Deck.card array) : result =
  let histogram = create_histogram h in
  check41_32_221_2111 histogram h

(*helper to factor out duplicated code below, if for some reason you didn't want it
  like this change it back -Abhi *)
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

let compare_hands (hand1 : result) (hand2 : result) : int =
  let h1 = compare_hands_helper hand1 in
  let h2 = compare_hands_helper hand2 in 
  if h1 > h2 then -1
  else if h2 > h1 then 1
  else
    match (hand1, hand2) with
    | (RoyalFlush, RoyalFlush) -> 0
    | (StraightFlush a, StraightFlush b) -> if a > b then -1 else if a = b then 0 else 1
    | (FourOfKind (a1, a2), FourOfKind (b1, b2)) -> if a1 > b1 then -1 else if b1 > a1 then 1 else if a2 > b2 then -1 else if b2 > a1 then 1 else 0
    | (FullHouse (a1, a2), FullHouse (b1, b2)) -> if a1 > b1 then -1 else if b1 > a1 then 1 else if a2 > b2 then -1 else if b2 > a1 then 1 else 0
    | (Flush (a1, a2, a3, a4, a5), Flush (b1, b2, b3, b4, b5)) -> if a1 > b1 then -1 else if b1 > a1 then 1 else if a2 > b2 then -1 else if b2 > a2 then 1 else if a3 > b3 then -1 else if b3 > a3 then 1 else if a4 > b4 then -1 else if b4 > a4 then 1 else if a5 > b5 then -1 else if b5 > a5 then 1 else 0
    | (Straight a, Straight b) -> if a > b then -1 else if b > a then 1 else 0
    | (ThreeOfKind (a1, a2, a3), ThreeOfKind (b1, b2, b3)) -> if a1 > b1 then -1 else if b1 > a1 then 1 else if a2 > b2 then -1 else if b2 > a2 then 1 else if a3 > b3 then -1 else if b3 > a3 then 1 else 0
    | (TwoPair (a1, a2, a3), TwoPair (b1, b2, b3)) -> if a1 > b1 then -1 else if b1 > a1 then 1 else if a2 > b2 then -1 else if b2 > a2 then 1 else if a3 > b3 then -1 else if b3 > a3 then 1 else 0
    | (OnePair (a1, a2, a3, a4), OnePair (b1, b2, b3, b4)) -> if a1 > b1 then -1 else if b1 > a1 then 1 else if a2 > b2 then -1 else if b2 > a2 then 1 else if a3 > b3 then -1 else if b3 > a3 then 1 else if a4 > b4 then -1 else if b4 > a4 then 1 else 0
    | (HighCard a, HighCard b) -> if a > b then -1 else if b > a then 1 else 0
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
  match List.sort compare_hands (List.map evaluate_hand (List.map Array.of_list (choose 5 (Array.to_list(Array.append hole community))))) with
  | [] -> failwith("empty")
  | h :: _ -> h

let evaluate_table (table : Table.table) : Table.person =
  let rec pairs (list : Table.person list) =
    match list with
    | [] -> []
    | h :: t -> (h, evaluate_hands [|fst (h.hand);snd (h.hand)|] (Array.of_list table.river)) :: pairs t in
  table.players |> pairs |> h_of_list |> extract_value |> fst