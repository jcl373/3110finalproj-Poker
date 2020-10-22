type pos =  
  | Dealer
  | BB
  | LB
  | Folded
  | Leave

(** The type [player] represents a player in the game. A player
    has a name, which is an identifier for the player, a hand, *)
type person = {name : string; mutable hand: Deck.card * Deck.card; 
               chips : int ref; mutable position : pos option } 

type table = {pot : int ref ; blinds: int * int; mutable river: Deck.card list; 
              mutable players : person list; mutable out_players : person list;
              mutable dealer : person option; mutable round_num : int}

val empty_table : int -> int -> table 

val new_player : string -> Deck.card -> Deck.card -> int -> person

val add_player : table -> person -> unit

val remove_player : table -> person -> unit

val choose_dealer : table -> unit

val next_round_prep : table -> unit

val extract_value : 'a option -> 'a

exception Empty