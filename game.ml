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

let getRank (card : Deck.card) = card.rank

(* TODO: add in helper methods to replace List.hd and List.nth cause Clarkson doesn't like them *)

(** [nth_of_list] returns the nth element of the list [lst]
    Returns an option as the list may not contain that number 
    [lst] is a valid list
    [n] is an int; represents the nth element
    [acc] is an int; the accumulator *)
let rec nth_of_list lst n acc = 
  match lst with
  | [] -> None
  | h :: t -> if acc = n then Some h else nth_of_list t n (acc+1)
;;

let evaluate_hand (h : Deck.card array) : result =
  let x : (int * int) list ref = ref [] in
  let add_to_histogram (card : Deck.card) : unit =
    match List.assoc_opt card.rank !x with
    | None -> x := (card.rank, 1) :: !x
    | Some i -> x := (card.rank, i + 1) :: List.remove_assoc card.rank !x in
  let histogram = 
    List.iter add_to_histogram (Array.to_list h);
    !x in

  let compareCards (c1 : Deck.card) (c2 : Deck.card) = 
    if getRank c1 = getRank c2 then 0
    (* else if getRank c1 = 1 then -1 (* These actually can cause aces to be in the wrong position*)
       else if getRank c2 = 1 then 1 *) 
    else if getRank c1 > getRank c2 then -1
    else 1 in
  let checkSTRAIGHT h = 
    let hSorted = (List.sort compareCards (Array.to_list h)) in
    if getRank (List.hd hSorted) - getRank (List.nth hSorted (List.length hSorted - 1)) = 4 then Straight (getRank (List.hd hSorted))
    else if getRank (List.nth hSorted (List.length hSorted - 1)) = 1 && getRank (List.hd hSorted) = 13 && getRank (List.hd hSorted) - getRank (List.nth hSorted (List.length hSorted - 2)) = 3 then Straight 1
    else HighCard (getRank (List.hd hSorted)) in
  let checkSTRAIGHTFLUSH h = 
    let hSorted = (List.sort compareCards (Array.to_list h)) in
    if getRank (List.hd hSorted) - getRank (List.nth hSorted (List.length hSorted - 1)) = 4 then StraightFlush (getRank (List.hd hSorted))
    else if getRank (List.nth hSorted (List.length hSorted - 1)) = 1 && getRank (List.hd hSorted) = 13 && getRank (List.hd hSorted) - getRank (List.nth hSorted (List.length hSorted - 2)) = 3 then RoyalFlush
    else Flush (getRank h.(0),getRank h.(1),getRank h.(2),getRank h.(3),getRank h.(4)) in

  let sameSuit (suit : char) (card : Deck.card) = card.suit = suit in
  let checkFLUSH h =
    if (Array.for_all (sameSuit 'C') h) then checkSTRAIGHTFLUSH h
    else if (Array.for_all (sameSuit 'D') h) then checkSTRAIGHTFLUSH h
    else if (Array.for_all (sameSuit 'H') h) then checkSTRAIGHTFLUSH h
    else if (Array.for_all (sameSuit 'S') h) then checkSTRAIGHTFLUSH h
    else checkSTRAIGHT h in

  let comparePairs pair1 pair2 =
    match (pair1, pair2) with
    | ((a,b),(x,y)) -> if b > y then -1 else if b < y then 1 else 
        (if a = 1 then -1 else if x = 1 then 1 else if a > x then -1 else if x > a then 1 else 0) in
  let check41_32_221_2111 x = 
    match List.sort comparePairs x with
    | [(x, 4); (y, 1)] -> FourOfKind (x, y)
    | [(x, 3); (y, 2)] -> FullHouse (x, y)
    | [(x, 3); (y, 1); (z, 1)] -> ThreeOfKind (x, y, z)
    | [(x, 2); (y, 2); (z, 1)] -> TwoPair (x, y, z)
    | [(x, 2); (y, 1); (z, 1); (w, 1)] -> OnePair (x,y,z,w)
    | _ -> checkFLUSH h in
  check41_32_221_2111 histogram

let compare_hands (hand1 : result) (hand2 : result) : int=
  let h1 =
    match hand1 with
    | RoyalFlush -> 10
    | StraightFlush _ -> 9
    | FourOfKind (_,_) -> 8
    | FullHouse (_,_) -> 7
    | Flush (_,_,_,_,_) -> 6
    | Straight _ -> 5
    | ThreeOfKind (_,_,_) -> 4
    | TwoPair (_,_,_) -> 3
    | OnePair (_,_,_,_) -> 2
    | HighCard _ -> 1 in
  let h2 =
    match hand2 with
    | RoyalFlush -> 10
    | StraightFlush _ -> 9
    | FourOfKind (_,_) -> 8
    | FullHouse (_,_) -> 7
    | Flush (_,_,_,_,_) -> 6
    | Straight _ -> 5
    | ThreeOfKind (_,_,_) -> 4
    | TwoPair (_,_,_) -> 3
    | OnePair (_,_,_,_) -> 2
    | HighCard _ -> 1 in
  if h1 > h2 then -1
  else if h2 > h1 then 1
  else 0 (* WIP *)

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