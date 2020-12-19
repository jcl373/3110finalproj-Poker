(** Handles deck creation, shuffling, and card pulling. *)

(** [card] represents a playing card with a rank from 1-13 and a suit
    denoted by the first character of the suit's name. A card with the rank of
    1 represents an ace, 11 is a Jack, 12 a Queen, and 13 a King.*)
type card = {rank : int; suit : char}

(** [deck] represents a mutable array of cards. *)
type deck = card array ref

(** [print_card card] is the string representation of the card [card]. *)
val print_card : card -> string

(** [empty] is the empty deck. *)
val empty : unit -> deck

(** [push card deck] creates a new array with value [card] at the 
    beginning of [deck] followed by the original values of [deck] before 
    returning it. *)
val push : card -> deck -> deck

(** [peek deck] is the first element of [deck]. *)
val peek : deck -> card

(** [pop deck] removes and then returns the first element of [deck]. *)
val pop : deck -> card

(** [shuffle deck] randomizes the deck [deck] using the Knuth shuffle algorithm 
    before returning the shuffled deck. *)
val shuffle : deck -> deck

(** [create] calls [create_help 1] to initialize a new deck with the standard 
    52-card deck in random order. *)
val create : unit -> deck

(** [create_size size] calls [create_help size] to intialize a new deck with 
    [size] number of standard 52-card decks in random order.
    [size] is an int >= 1. *)
val create_size : int -> deck