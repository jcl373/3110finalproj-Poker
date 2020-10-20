(** The type [card] represents a playing card with a rank from 1-13 and a suit
     denoted by the first character of the suit's name. A card with the rank of
     1 represents an ace, 11 is a Jack, 12 a Queen, and 13 a King.*)
type card = {rank : int; suit : char}

(** The type [deck] represents a mutable array of cards. *)
type deck = card array ref

(* [empty] is the empty deck *)
val empty : deck

(* To push [c] onto [d], we create a new array with value [c] at the 
   beginning followed by the values of [d] before returning [d]. *)
val push : card -> deck -> deck

(* [peek d] is the first element of [d].*)
val peek : deck -> card

(* [pop d] removes and then returns the first element of [d]. *)
val pop : deck -> card

(* [shuffle d] randomizes the deck [d] using the Knuth shuffle algorithm before returning the shuffled deck. *)
val shuffle: deck -> deck

(* [create] calls [create_help 1] to initialize a new deck with the standard 
   52-card deck in random order. *)
val create : deck

(* [create_size s] calls [create_help s] to intialize a new deck with s number 
   of standard 52-card decks in random order.
   [s] is an int >= 1*)
val create_size : int -> deck



