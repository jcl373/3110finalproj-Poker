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

val evaluate_hand : Deck.card array -> result

val compare_hands : result -> result -> int 

(**[evaluate_hands] takes in a player's pair of cards [hole] and 
  the current community cards on the table [community] and computes the players 
  best possible hand from this,
  Returns a result
  [hole] is a valid Deck.card array of length 2
  [community] is a valid Deck.card array of length 5 *)
val evaluate_hands : Deck.card array -> Deck.card array -> result

val evaluate_table : Table.table -> Table.person