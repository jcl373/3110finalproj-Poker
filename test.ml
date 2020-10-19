open OUnit2
open Deck

let deck_tests = [




]


let suite =
  "test suite"  >::: List.flatten [
    deck_tests;
  ]

let _ = run_test_tt_main suite