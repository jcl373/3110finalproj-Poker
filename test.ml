open OUnit2
open Deck


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
      assert_equal expected_output ((!) (push c d)) ~printer:(pp_array pp_card))

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
let ace_spade_deck = ref ace_spade_array (*Deck containing ace of spades *)

let deck_tests =
  [
    push_test "pushing one card on empty" ace_spade Deck.empty ace_spade_array;
    push_test "pushing one card on non-empty deck" king_spade ace_spade_deck ace_king_spade_array;
    pop_test "poping one card" ace_spade_deck ace_spade;
  ]

let suite =
  "test suite"  >::: List.flatten [
    deck_tests
  ]

let _ = run_test_tt_main suite