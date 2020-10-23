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

val evaluate_hands : Deck.card array -> Deck.card array -> result

val evaluate_table : Table.table -> Table.person