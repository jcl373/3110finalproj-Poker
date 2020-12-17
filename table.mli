(** Handles the gametable, round changes, and players. *)

(** [pos] represents a type of position at the table. *)
type pos =  
  | Dealer
  | BB
  | LB
  | Folded
  | Leave
  | AllIn of int

(** Raised when an unknown response is encountered. *)
exception InvalidResponse

(** [player] represents a player in the game.
    [name] is a string identifier for the player.
    [hand] is mutable pair of cards in the player's hand.
    [chips] is a int reference representing the player's money.
    [last_bet] is the last bet the player made. 
    [position] is a mutable position option representing the player's place at 
    the table. 
    [location] is a pair of ints representing the pixel location of the player 
    on the screen. *)
type person = {name : string; 
               mutable hand: Deck.card * Deck.card; 
               chips : int ref; 
               mutable last_bet : int;
               mutable position : pos option; 
               location : int * int } 

(** [table] represents a poker table.
    [pot] is a mutable int ref which represents the amount in the pot.
    [blinds] is an int tuple representing the SB and the BB respectively.
    [river] are the river cards in the middle of the table
    [players] are the players in the game.
    [in_players] are the playres who are still in the game.
    [out_players] are the players which have left the game.
    [dealer] is the person who is the dealer.
    [round_num] represents the round number of the entire game.
    [side_pots] handles any side pots.
    [last_bet] is the person who made the last bet.
    [last_call] is the latest call value. *)
type table = {mutable pot : int ref ; blinds: int * int; 
              mutable river: Deck.card list; 
              mutable players : person list; mutable in_players : person list; 
              mutable out_players : person list;
              mutable dealer : person option; mutable round_num : int;
              mutable side_pots : (int * person list) list;
              mutable last_bet : person option;
              mutable last_call : int;}

(** [empty_table sb bb] initalizes an empty table with small blind [sb] and 
    big blind [bb]. *)
val empty_table : int -> int -> table 

(** [new_player nm c1 c2 start_amt loc] creates a new player with name [nm], 
    hand [c1 * c2], starting amount [start_amt], and location [loc]. *)
val new_player : string -> Deck.card -> Deck.card -> int -> int * int -> person

(** [add_player tab player] adds a new person 
    [player] to the poker table [table]. *)
val add_player : table -> person -> unit

(** [remove_player tab player] removes a new person 
    [player] to the poker table [table]. *)
val remove_player : table -> person -> unit

(** [choose_dealer table] chooses a new dealer in the current table.
    Dealer responbilities are assigned clockwise, and at the beginning 
    of the round a dealer is chosen arbitrarily. *)
val choose_dealer : table -> unit

(** [next_round_prep table] makes the between round game changes on table 
    [table]. *)
val next_round_prep : table -> unit

(** [next_br_prep table] makes the between betting-round game changes on table 
    [table]. *)
val next_br_prep : table -> unit

(** [side_pots_prep table round] updates the side pots for table [table] in 
    round [round] based on the players who went all-in. *)
val side_pots_prep : table -> int -> unit

(** [remove_folded lst] removes the players who have folded from list of
    players [lst]. *)
val remove_folded : person list -> person list

(** [set_hand p c1 c2] sets player [p]'s hand to [c1 * c2]. *)
val set_hand : person -> Deck.card -> Deck.card -> unit

(** [auto_remove table p] removes player [p] from table [table] if they have 
    too few chips. *)
val auto_remove : table -> person -> unit

(** [winner winner gametable gamedeck f i] announces the winner [winner] of 
    table [gametable] and resets [gamedeck]. *)
val winner : person -> table -> Deck.deck -> (int -> unit) -> int -> unit

(** [end_prompt x f i] closes the game. *)
val end_prompt : int -> (int -> unit) -> int -> unit

(** [last_one_wins table gamedeck round i] determines the winner if there is 
    onle one player left in [table]. *)
val last_one_wins : table -> 'a -> 'b -> 'c -> person option

(** [min_players gametable f i] ends the game if there are less than 2 players 
    in table [gametable]. *)
val min_players : table -> (int -> unit) -> int -> unit

(* remove following vals once replaced in table *)

(** [n_of_list] returns the nth element of the list [lst].
    Returns an option as the list may not contain that number.
    [lst] is a valid list.
    [n] is an int; represents the nth element. *)
val n_of_list : 'a list -> int -> 'a option

(** [extract_value opt] gives the value for option [opt] or raises [Empty] if 
    [opt] is None. *)
val extract_value : 'a option -> 'a

(** [find_list lst x] finds value [x] in list [lst]. *)
val find_list : 'a list -> 'a -> int option

(** [init_commcard table deck] initializes the first three cards in the river 
    at table [table] from deck [deck]. *)
val init_commcard : table -> Deck.deck -> unit

(** [add_commcard table deck] adds a new card to the river in table [table] 
    from deck [deck]. *)
val add_commcard : table -> Deck.deck -> unit

(** Raised when an option is None. *)
exception Empty