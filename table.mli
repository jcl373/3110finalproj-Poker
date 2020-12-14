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
               chips : int ref; mutable last_bet : int;
               mutable position : pos option; 
               location : int * int } 

(** The type [table] represents a poker table.
    [pot] is a mutable int ref which represents the amount in the pot
    [blinds] is an int tuple representing the SB and the BB respectively
    [river] are the river cards in the middle of the table
    [players] are the players in the game
    [in_players] is ....
    [out_players] are the players which have left the game
    [dealer] is the person who is the dealer
    [round_num] represents the round number of the entire game
    [side_pots] handles any side potss
    [last_bet] is the person who made the ...
    [last_call] is the latest call value *)
type table = {mutable pot : int ref ; blinds: int * int; 
              mutable river: Deck.card list; 
              mutable players : person list; mutable in_players : person list; 
              mutable out_players : person list;
              mutable dealer : person option; mutable round_num : int;
              mutable side_pots : (int * person list) list;
              mutable last_bet : person option;
              mutable last_call : int;}

(**[empty_table sb bb] initalizes an empty table with sb as the 
    small blind value and bb as the big blind value *)
val empty_table : int -> int -> table 


val new_player : string -> Deck.card -> Deck.card -> int -> int * int -> person

(** [add_player tab player] adds a new person 
    [player] to the poker table [table] *)
val add_player : table -> person -> unit

(** [remove_player tab player] removes a new person 
    [player] to the poker table [table] *)
val remove_player : table -> person -> unit

(** [choose_dealer table] chooses a new dealer in the current table
    Dealer responbilities are assigned clockwise, and at the beginning 
    of the round a dealer is chosen arbitrarily *)
val choose_dealer : table -> unit

val next_round_prep : table -> unit

val next_br_prep : table -> unit

val side_pots_prep : table -> int -> unit

val n_of_list : 'a list -> int -> 'a option

val h_of_list : 'a list -> 'a option

val find_list : 'a list -> 'a -> int option

(** [remove_folded lst] removes the players who have folded from list of
    players lst *)
val remove_folded : person list -> person list

val extract_value : 'a option -> 'a

val set_hand : person -> Deck.card -> Deck.card -> unit

val init_commcard : table -> Deck.deck -> unit

val add_commcard : table -> Deck.deck -> unit

val auto_remove : table -> person -> unit

val winner : person -> table -> Deck.deck -> (int -> unit) -> int -> unit

val end_prompt : int -> (int -> unit) -> int -> unit

val last_one_wins : table -> 'a -> 'b -> 'c -> person option

val min_players : table -> (int -> unit) -> int -> unit



exception Empty