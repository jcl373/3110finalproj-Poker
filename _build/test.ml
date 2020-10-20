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
let ace_spade_array =  [|ace_spade|] (*Array containing ace of spades *)
let ace_king_spade_array = [|ace_spade;king_spade|] (*Array with King/ace of spades *)
let ace_spade_deck = ref [|ace_spade|] (*Deck containing ace of spades *)

let deck_tests =
  [
    push_test "pushing one card on empty" ace_spade Deck.empty ace_spade_array;
    pop_test "popping one card" ace_spade_deck ace_spade;
  ]

(** [pp_result r] pretty-prints result [r]. *)
let pp_result (r : Game.result) = 
  match r with
  | RoyalFlush -> "Royal Flush"
  | StraightFlush a -> "Straight Flush " ^ string_of_int a 
  | FourOfKind (a, b) -> "Four of a kind " ^ string_of_int a ^ " " ^ string_of_int b
  | FullHouse (a, b) -> "Full house " ^ string_of_int a ^ " " ^ string_of_int b 
  | Flush (a, b, c, d, e) -> "Flush " ^ string_of_int a ^ " " ^ string_of_int b ^ " " ^ string_of_int c ^ " " ^ string_of_int d ^ " " ^ string_of_int e
  | Straight a -> "Straight " ^ string_of_int a
  | ThreeOfKind (a, b, c) -> "Three of a kind " ^ string_of_int a ^ " " ^ string_of_int b ^ " " ^ string_of_int c
  | TwoPair (a, b, c) -> "Two pairs " ^ string_of_int a ^ " " ^ string_of_int b ^ " " ^ string_of_int c
  | OnePair (a, b, c, d)-> "One Pair " ^ string_of_int a ^ " " ^ string_of_int b ^ " " ^ string_of_int c ^ " " ^ string_of_int d
  | HighCard a -> "High card " ^ string_of_int a

(** [hand_test name hand expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [evaluate_hand hand]. *)
let hand_test
    (name : string) 
    (hand : Deck.card array)
    (expected_output : Game.result) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Game.evaluate_hand hand) ~printer:pp_result)

let hand_tests =
  [
    hand_test "Royal flush" [|{rank = 1;suit='C'};{rank = 13;suit='C'};
                              {rank = 12;suit='C'};{rank = 11;suit='C'};
                              {rank = 10; suit = 'C'}|] RoyalFlush;
    push_test "pushing one card on non-empty deck" king_spade (ref ace_spade_array) ace_king_spade_array;
    pop_test "poping one card" (ref ace_spade_array) ace_spade;
  ]

let suite =
  "test suite"  >::: List.flatten [
    deck_tests;
    hand_tests
  ]

let _ = run_test_tt_main suite