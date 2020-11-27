open OUnit2
open Deck
open Game

(** [cmp_arrays a1 a2] compares two arrays to see whether
    they are equivalent arrays.  They must contain the same elements,
    though not necessarily in the same order. *)
let cmp_arrays a1 a2 =
  let uniq1 = Array.sort compare a1 in
  let uniq2 = Array.sort compare a2 in
  (* List.length a1 = List.length uniq1
     &&
     List.length a2 = List.length uniq2
     && *)
  uniq1 = uniq2

(** [pp_array pp_elt arr] pretty-prints array [arr], using [pp_elt]
    to pretty-print each element of [arr]. *)
let pp_array (pp_elt : 'a -> string)  (arr : 'a array) = String.concat " " (Array.to_list (Array.map pp_elt arr))

(** [pp_card c] pretty-prints card [c]. *)
let pp_card (c : Deck.card) = "{" ^ string_of_int c.rank ^ " " ^ Char.escaped c.suit ^ "}"

(*Tests for Deck.ml start here *)

(** [print_card_test name c expected_output] constructs an OUnit 
  test named name which asserts the equality of [expected_output]
    with [print_card c] *)
let print_card_test 
    (name : string)
    (c: Deck.card)
    (expected_output : string) : test = 
    name >:: (fun _ -> 
      assert_equal expected_output (print_card c))

(** [push_test name c d expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [push c d]. *)
let push_test
    (name : string) 
    (c : Deck.card) 
    (d : Deck.deck)
    (expected_output : card array) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output ((!) (push c d)) ~cmp:(cmp_arrays) ~printer:(pp_array pp_card))

(** [peek_test name d expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [peek d]. *)
let peek_test
    (name : string) 
    (d : Deck.deck)
    (expected_output : card) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (peek d) ~printer:pp_card)

(** [pop_test name d expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [pop d]. *)
let pop_test
    (name : string) 
    (d : Deck.deck)
    (expected_output : card) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pop d) ~printer:pp_card)

(** [pop_test name d] constructs an OUnit
    test named [name] that asserts physical inequality between [d] and 
    [shuffle d]. *)
let shuffle_test
    (name : string) 
    (d : Deck.deck) : test = 
  name >:: (fun _ -> 
      assert_bool "" (!d <> ((!) (shuffle d))))

(** [deck_test name d expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [d]. *)
let deck_test
    (name : string) 
    (d : Deck.deck)
    (expected_output : card array) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output ((!) d) ~printer:(pp_array pp_card))

let ace_spade = {rank = 1 ; suit = 'S'} (* Ace of Spades Card *)
let king_spade = {rank = 13 ; suit = 'S'} (* King of Spades card *)
let king_hearts = {rank = 13; suit = 'H'} (* King of Hearts*)
let ace_spade_array =  [|ace_spade|] (*Array containing ace of spades *)
let ace_king_spade_array = [|ace_spade; king_spade|] 
let multi_deck = ref [|king_hearts;ace_spade;king_spade|]
let ace_spade_deck = ref [|ace_spade|] (*Deck containing ace of spades *)
let empty = empty

let rec pp_result_helper = function
| [] -> failwith "empty list should never be called"
| h::[] -> string_of_int h ^ ""
| h::t -> string_of_int h ^ " " ^ pp_result_helper t

(** [pp_result r] pretty-prints result [r]. *)
let pp_result (r : result) = 
  match r with
  | RoyalFlush -> "Royal Flush"
  | StraightFlush a -> "Straight Flush " ^ string_of_int a 
  | FourOfKind (a, b) -> 
    "Four of a kind " ^ pp_result_helper [a; b]
  | FullHouse (a, b) -> 
    "Full house " ^ pp_result_helper [a; b]
  | Flush (a, b, c, d, e) -> 
    "Flush " ^ pp_result_helper [a; b; c; d; e]
  | Straight a -> 
    "Straight " ^ string_of_int a
  | ThreeOfKind (a, b, c) -> 
    "Three of a kind " ^ pp_result_helper [a; b; c]
  | TwoPair (a, b, c) -> 
    "Two pairs " ^ pp_result_helper [a; b; c]
  | OnePair (a, b, c, d)-> 
    "One Pair " ^ pp_result_helper [a; b; c;d ]
  | HighCard a -> "High card " ^ string_of_int a

  let deck_tests =
  [
    print_card_test "Testing king of hearts" 
                    {rank = 13; suit = 'H'} "King of Hearts";
    print_card_test "Testing king of spades" 
                    {rank = 13; suit = 'S'} "King of Spades";
    print_card_test "Testing king of diamonds" 
                    {rank = 13; suit = 'D'} "King of Diamonds";
    print_card_test "Testing king of clubs" 
                    {rank = 13; suit = 'C'} "King of Clubs";
    print_card_test "Testing queen hearts" 
                    {rank = 12; suit = 'H'} "Queen of Hearts";
    print_card_test "Testing queen spades" 
                    {rank = 12; suit = 'S'} "Queen of Spades";
    print_card_test "Testing queen diamonds" 
                    {rank = 12; suit = 'D'} "Queen of Diamonds";
    print_card_test "Testing queen clubs" 
                    {rank = 12; suit = 'C'} "Queen of Clubs";
    print_card_test "Testing jack hearts" 
                    {rank = 11; suit = 'H'} "Jack of Hearts";
    print_card_test "Testing jack spades" 
                    {rank = 11; suit = 'S'} "Jack of Spades";
    print_card_test "Testing jack diamonds" 
                    {rank = 11; suit = 'D'} "Jack of Diamonds";
    print_card_test "Testing jack clubs" 
                    {rank = 11; suit = 'C'} "Jack of Clubs";
    print_card_test "Testing ace hearts" 
                    {rank = 1; suit = 'H'} "Ace of Hearts";
    print_card_test "Testing ace spades" 
                    {rank = 1; suit = 'S'} "Ace of Spades";
    print_card_test "Testing ace diamonds" 
                    {rank = 1; suit = 'D'} "Ace of Diamonds";
    print_card_test "Testing ace clubs" 
                    {rank = 1; suit = 'C'} "Ace of Clubs";
    print_card_test "Testing 9 hearts" 
                    {rank = 9; suit = 'H'} "9 of Hearts";
    print_card_test "Testing 10 spades" 
                    {rank = 10; suit = 'S'} "10 of Spades";
    print_card_test "Testing 7 diamonds" {rank = 7; suit = 'D'} "7 of Diamonds";
    print_card_test "Testing 6 clubs" {rank = 6; suit = 'C'} "6 of Clubs";
    push_test "pushing one card on empty" 
              ace_spade (Deck.empty ()) ace_spade_array;
    push_test "pushing a card on non empty deck" 
              king_spade ace_spade_deck ace_king_spade_array;
    peek_test "testing deck with one card" ace_spade_deck ace_spade;
    peek_test "testing deck with multiple cards" multi_deck king_hearts;
    pop_test "popping one card" ace_spade_deck ace_spade;
  ]

(** [hand_test name hand expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [evaluate_hand hand]. *)

(*Tests for Deck.ml end here *)

(*Tests for table.ml start here *)
(*Tests for table.ml end here *)

(*Tests for game.ml start here *)
let hand_test
    (name : string) 
    (hand : Deck.card array)
    (expected_output : Game.result) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Game.evaluate_hand hand) ~printer:pp_result)

let hand_tests =
  [
    hand_test "Royal flush C" [|{rank = 1;suit='C'};{rank = 13;suit='C'};
                                {rank = 12;suit='C'};{rank = 11;suit='C'};
                                {rank = 10; suit = 'C'}|] RoyalFlush;

    hand_test "Straight Flush C" [|{rank = 9;suit='C'};{rank = 13;suit='C'};
                                   {rank = 12;suit='C'};{rank = 11;suit='C'};
                                   {rank = 10; suit = 'C'}|] (StraightFlush 13);

    hand_test "Royal flush H" [|{rank = 1;suit='H'};{rank = 13;suit='H'};
                                {rank = 12;suit='H'};{rank = 11;suit='H'};
                                {rank = 10; suit = 'H'}|] RoyalFlush;

    hand_test "Straight Flush H" [|{rank = 2;suit='H'};{rank = 4;suit='H'};
                                   {rank = 6;suit='H'};{rank = 3;suit='H'};
                                   {rank = 5; suit = 'H'}|] (StraightFlush 6); 

    hand_test "Straight 2 types" [|{rank = 2;suit='C'};{rank = 4;suit='H'};
                                   {rank = 6;suit='C'};{rank = 3;suit='H'};
                                   {rank = 5; suit = 'H'}|] (Straight 6); 

    hand_test "Straight 3 types" [|{rank = 7;suit='C'};{rank = 4;suit='H'};
                                   {rank = 6;suit='D'};{rank = 8;suit='D'};
                                   {rank = 5; suit = 'H'}|] (Straight 8); 

    hand_test "Straight 4 types" [|{rank = 11;suit='C'};{rank = 10;suit='S'};
                                   {rank = 9;suit='D'};{rank = 8;suit='D'};
                                   {rank = 7; suit = 'H'}|] (Straight 11);    

    hand_test "High Ace" [|{rank = 7;suit='C'};{rank = 10;suit='S'};
                           {rank = 9;suit='D'};{rank = 8;suit='D'};
                           {rank = 1; suit = 'H'}|] (HighCard 1);   

    hand_test "One Pair" [|{rank = 7;suit='C'};{rank = 7;suit='S'};
                           {rank = 9;suit='D'};{rank = 8;suit='D'};
                           {rank = 1; suit = 'H'}|] (OnePair (7, 1, 9, 8));                         

]

let compare_hand_test
    (name : string) 
    (hand1 : Game.result)
    (hand2 : Game.result)
    (e_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal e_output (compare_hands hand1 hand2)  ~printer:string_of_int)

let compare_hand_tests =
  [   
    compare_hand_test "Different hands, hand1 better" 
                      (RoyalFlush) (StraightFlush 7) ~-1;
    compare_hand_test "Different hands, hand2 better" 
                      (StraightFlush 7) (RoyalFlush) 1;
    compare_hand_test "both royalflush" (RoyalFlush) (RoyalFlush) 0;
    compare_hand_test "both sflush, hand1 better" 
                      (StraightFlush 9) (StraightFlush 7) ~-1;
    compare_hand_test "both sflush, hand2 better" 
                      (StraightFlush 7) (StraightFlush 9) 1;
    compare_hand_test "both sflush, equal" 
                      (StraightFlush 7) (StraightFlush 7) 0;
    compare_hand_test "both 4kind, hand1 better" 
                      (FourOfKind (9,3)) (FourOfKind (7,2)) ~-1;
    compare_hand_test "both 4kind, hand2 better" 
                      (FourOfKind (7,3)) (FourOfKind (11,2)) 1;
    compare_hand_test "both 4kind, hand1 better,4pair same" 
                      (FourOfKind (9,3)) (FourOfKind (9,2)) ~-1;
    compare_hand_test "both 4kind, hand2 better,4pair same" 
                      (FourOfKind (7,3)) (FourOfKind (7,6)) 1;
    compare_hand_test "both 4kind, equal" 
                      (FourOfKind (7,2)) (FourOfKind (7,2)) 0;
    compare_hand_test "both fhouse, hand1 better, diff 3kind" 
                      (FullHouse (8,3)) (FullHouse (7,4)) ~-1;
    compare_hand_test "both fhouse, hand1 better, same 3kind" 
                      (FullHouse (7,3)) (FullHouse (7,2)) ~-1;
    compare_hand_test "both fhouse, hand2 better, diff 3kind" 
                      (FullHouse (8,3)) (FullHouse (9,4)) 1;
    compare_hand_test "both fhouse, hand2 better, same 3kind" 
                      (FullHouse (11,3)) (FullHouse (11,6)) 1;
    compare_hand_test "both fhouse, equal" 
                      (FullHouse (7,2)) (FullHouse (7,2)) 0;
    compare_hand_test "both flush, hand2 better" 
                      (Flush (8,6,4,2,1)) (Flush(10,8,5,3,2)) 1;
    compare_hand_test "both flush, hand1 better" 
                      (Flush (10,8,5,3,2)) (Flush (8,6,4,2,1)) ~-1;
    compare_hand_test "both flush, equal" 
                      (Flush (8,6,4,2,1)) (Flush (8,6,4,2,1)) 0;
    compare_hand_test "both straight, hand1 better" 
                      (Straight 9) (Straight 7) ~-1;
    compare_hand_test "both straight, hand2 better" 
                      (Straight 7) (Straight 9) 1;
    compare_hand_test "both straight, equal" (Straight 7) (Straight 7) 0;
    compare_hand_test "both 3kind, hand1 better" 
                      (ThreeOfKind (7,2,1)) (ThreeOfKind (5,4,3)) ~-1;
    compare_hand_test "both 3kind, hand2 better" 
                      (ThreeOfKind (5,4,3)) (ThreeOfKind(7,2,1)) 1;
    compare_hand_test "both 3kind, equal" 
                      (ThreeOfKind (5,4,3)) (ThreeOfKind (5,4,3)) 0;
    compare_hand_test "both 2pair, hand1 better" 
                      (TwoPair (7,2,1)) (TwoPair (5,4,3)) ~-1;
    compare_hand_test "both 2pair, hand2 better" 
                      (TwoPair (5,4,3)) (TwoPair(7,2,1)) 1;
    compare_hand_test "both 2pair, equal" (TwoPair (5,4,3)) (TwoPair (5,4,3)) 0;
    compare_hand_test "both pair, hand1 better" 
                      (OnePair (7,4,2,1)) (OnePair (5,4,3,2)) ~-1;
    compare_hand_test "both pair, hand2 better" 
                      (OnePair (5,4,3,1)) (OnePair(7,4,2,1)) 1;
    compare_hand_test "both pair, equal" 
                      (OnePair (10,5,4,3)) (OnePair (10,5,4,3)) 0;
    compare_hand_test "both hcard, hand1 better" (HighCard 10) (HighCard 7) ~-1;
    compare_hand_test "both hcard, hand2 better" (HighCard 3) (HighCard 5) 1;
    compare_hand_test "both hcard, equal" (HighCard 6) (HighCard 6) 0;
]

let evaluate_hands_test
    (name : string) 
    (hole : Deck.card array)
    (community : Deck.card array)
    (e_output : result) : test = 
  name >:: (fun _ -> 
      assert_equal e_output (evaluate_hands hole community) ~printer:pp_result)

  let evaluate_hands_tests = 
    [
      evaluate_hands_test "High card evaluated" 
                          [|{rank = 2;suit='C'};{rank = 4;suit='S'}|] 
                          [|{rank = 6;suit='C'};{rank = 8;suit='S'};
                          {rank = 12;suit='D'};{rank = 10;suit='D'};
                          {rank = 1; suit = 'H'}|] (HighCard 12); 
      evaluate_hands_test "OnePair evaluated" 
                          [|{rank = 2;suit='C'};{rank = 4;suit='S'}|] 
                          [|{rank = 9;suit='C'};{rank = 5;suit='S'};
                          {rank = 11;suit='D'};{rank = 8;suit='D'};
                          {rank = 11; suit = 'H'}|] (OnePair (11,9,8,5)); 
      evaluate_hands_test "TwoPair evaluated" 
                          [|{rank = 2;suit='C'};{rank = 4;suit='S'}|] 
                          [|{rank = 9;suit='C'};{rank = 5;suit='S'};
                          {rank = 11;suit='D'};{rank = 9;suit='D'};
                          {rank = 11; suit = 'H'}|] (TwoPair (11,9,5)); 
      evaluate_hands_test "ThreeKind evaluated" 
                          [|{rank = 2;suit='C'};{rank = 4;suit='S'}|] 
                          [|{rank = 9;suit='C'};{rank = 5;suit='S'};
                          {rank = 11;suit='D'};{rank = 11;suit='C'};
                          {rank = 11; suit = 'H'}|] (ThreeOfKind (11,9,5)); 
      evaluate_hands_test "Straight evaluated" 
                          [|{rank = 7;suit='S'};{rank = 4;suit='S'}|] 
                          [|{rank = 8;suit='C'};{rank = 3;suit='S'};
                          {rank = 9;suit='D'};{rank = 10;suit='C'};
                          {rank = 11; suit = 'H'}|] (Straight (11)); 
      evaluate_hands_test "Flush evaluated" 
                          [|{rank = 6;suit='H'};{rank = 3;suit='H'}|] 
                          [|{rank = 2;suit='H'};{rank = 3;suit='S'};
                          {rank = 9;suit='D'};{rank = 10;suit='H'};
                          {rank = 11; suit = 'H'}|] (Flush (6,3,2,10,11)); 
      evaluate_hands_test "Fullhouse evaluated" 
                          [|{rank = 6;suit='H'};{rank = 6;suit='D'}|] 
                          [|{rank = 2;suit='D'};{rank = 3;suit='C'};
                          {rank = 6;suit='C'};{rank = 10;suit='H'};
                          {rank = 10; suit = 'D'}|] (FullHouse (6,10)); 
      evaluate_hands_test "4Kind evaluated" 
                          [|{rank = 11;suit='S'};{rank = 4;suit='S'}|] 
                          [|{rank = 9;suit='C'};{rank = 5;suit='S'};
                          {rank = 11;suit='D'};{rank = 11;suit='C'};
                          {rank = 11; suit = 'H'}|] (FourOfKind (11,9));
      evaluate_hands_test "4Kind evaluated" 
                          [|{rank = 8;suit='S'};{rank = 7;suit='S'}|] 
                          [|{rank = 9;suit='C'};{rank = 5;suit='S'};
                          {rank = 10;suit='S'};{rank = 9;suit='S'};
                          {rank = 11; suit = 'S'}|] (StraightFlush (11));
      evaluate_hands_test "4Kind evaluated" 
                          [|{rank = 12;suit='S'};{rank = 10;suit='S'}|] 
                          [|{rank = 3;suit='C'};{rank = 5;suit='D'};
                          {rank = 11;suit='S'};{rank = 13;suit='S'};
                          {rank = 1; suit = 'S'}|] (RoyalFlush);
                      
    ]

(* Tests for game.ml end here *)
let suite =
  "test suite"  >::: List.flatten [
    deck_tests;
    hand_tests;
    compare_hand_tests;
    evaluate_hands_tests
  ]

let _ = run_test_tt_main suite