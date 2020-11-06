type pos =  
  | Dealer
  | BB
  | LB
  | Folded
  | Leave
  | AllIn of int

exception InvalidResponse

(** The type [player] represents a player in the game. A player
    has a name, which is an identifier for the player, a hand, *)
type person = {name : string; mutable hand: Deck.card * Deck.card; 
               chips : int ref; mutable position : pos option } 

type table = {mutable pot : int ref ; blinds: int * int; 
              mutable river: Deck.card list; 
              mutable players : person list; mutable in_players : person list; 
              mutable out_players : person list;
              mutable dealer : person option; mutable round_num : int;
              mutable side_pots : (int * person list) list;}

val empty_table : int -> int -> table 

val new_player : string -> Deck.card -> Deck.card -> int -> person

val add_player : table -> person -> unit

val remove_player : table -> person -> unit

val choose_dealer : table -> unit

val next_round_prep : table -> unit

val next_br_prep : table -> unit

val side_pots_prep : table -> int -> unit

val find_list : 'a list -> 'a -> int option

val n_of_list : 'a list -> int -> 'a option

val extract_value : 'a option -> 'a

val set_hand : person -> Deck.card -> Deck.card -> unit

val init_commcard : table -> Deck.deck -> unit

val add_commcard : table -> Deck.deck -> unit

val auto_remove : table -> person -> unit

val winner : person -> table -> Deck.deck -> (int -> unit) -> int -> unit

val end_prompt : int -> (int -> unit) -> int -> unit

val last_one_wins : table -> Deck.deck -> (int -> unit) -> int -> unit

val min_players : table -> (int -> unit) -> int -> unit



exception Empty