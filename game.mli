(** Handles hand evaluation and comparison. *)


(** [result] is a type of poker hand, and it carries integers which represent
    the cards in the hand.
    Example: Onepair of (7,5,4,3) represents a hand with 2 7's, a 5, a 4, and a 
    3 card (suit is irrelevant). *)
type result =
  | RoyalFlush
  | StraightFlush of int
  | FourOfKind of int * int
  | FullHouse of int * int
  | Flush of int * int * int * int * int
  | Straight of int
  | ThreeOfKind of int * int * int
  | TwoPair of int * int * int
  | OnePair of int * int * int * int
  | HighCard of int 

(** [evaluate_hand hand] takes in a Deck.card array [hand] of length 5 and 
    returns the type of result which it represents. *)
val evaluate_hand : Deck.card array -> result

(** [compare_hands hand1 hand2] returns an integer based on which one of 2 hands 
    is better in terms of poker rules. If [hand1] has a higher score than 
    [hand2], it returns -1. If [hand2] is better than [hand1], it returns 1. 
    If they have the same "value", then it returns 0. *)
val compare_hands : result -> result -> int 

(** [evaluate_hands hole community] computes the best possible hand from [hole] 
    and [community] and then returns the result.
    [hole] is a valid Deck.card array of length 2.
    [community] is a valid Deck.card array of length 5. *)
val evaluate_hands : Deck.card array -> Deck.card array -> result

(** [evaluate_table table] evaluates the table [table] and returns the player 
    with the best hand. 
    [table] is a table with at least one player in it. *)
val evaluate_table : Table.table -> Table.person