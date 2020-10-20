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

let gethistogram (h : Deck.card array) =
  let x : (int * int) list ref = ref [] in
  let add_to_histogram (card : Deck.card) : unit =
    match List.assoc_opt card.rank !x with
    | None -> x := (card.rank, 1) :: !x
    | Some i -> x := (card.rank, i + 1) :: List.remove_assoc card.rank !x
  in
  List.iter add_to_histogram (Array.to_list h);
  !x

let compareCards (c1 : Deck.card) (c2 : Deck.card) = if getRank c1 > getRank c2 then -1
  else if getRank c1 = getRank c2 then 0 else 1

let evaluate_hand (h : Deck.card array) : result =
  let x : (int * int) list ref = ref [] in
  let add_to_histogram (card : Deck.card) : unit =
    match List.assoc_opt card.rank !x with
    | None -> x := (card.rank, 1) :: !x
    | Some i -> x := (card.rank, i + 1) :: List.remove_assoc card.rank !x in
  let histogram = 
    List.iter add_to_histogram (Array.to_list h);
    !x in
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
  let check41_32_221_2111 x =
    match List.sort compare x with
    | [(x, 4); (y, 1)] -> FourOfKind (x, y)
    | [(x, 1); (y, 4)] -> FourOfKind (y, x)
    | [(x, 3); (y, 2)] -> FullHouse (x, y)
    | [(x, 2); (y, 3)] -> FullHouse (y, x)
    | [(x, 3); (y, 1); (z, 1)] -> ThreeOfKind (x, z, y)
    | [(x, 1); (y, 3); (z, 1)] -> ThreeOfKind (y, z, x)
    | [(x, 1); (y, 1); (z, 3)] -> ThreeOfKind (z, y, x)
    | [(x, 2); (y, 2); (z, 1)] -> TwoPair (y, x, z)
    | [(x, 2); (y, 1); (z, 2)] -> TwoPair (z, x, y)
    | [(x, 1); (y, 2); (z, 2)] -> TwoPair (z, y, x)
    | [(x, 2); (y, 1); (z, 1); (w, 1)] -> OnePair (x,w,z,y)
    | [(x, 1); (y, 2); (z, 1); (w, 1)] -> OnePair (y,w,z,x)
    | [(x, 1); (y, 1); (z, 2); (w, 1)] -> OnePair (z,w,y,x)
    | [(x, 1); (y, 1); (z, 1); (w, 2)] -> OnePair (w,z,y,x)
    | _ -> checkFLUSH h in
  check41_32_221_2111 histogram


