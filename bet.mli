(** [bag] represents a players bag of how much money they have 
    available at a given time to use.
    This can change depending on the instance of the game and round. *)
type bag = int ref

(** [pot] represents the pot of money that has been wagered by all players 
    This can change depending on the instance of the game and round. *)
type pot = int ref

(** [choice] represents the choice of bet that the player has chosen to
    do at their given turn. *)
type choice = 
  | Check
  | Fold
  | Bet of int 
  | Call of int
  | Raise of int
  | AllIn of int

exception InvalidResponse

exception InvalidWager

(** [amount] returns the amount of money in a given bag. *)
val amount : bag -> int

val add : bag -> int -> bag 

val set : int -> bag -> unit

val empty_bag : unit -> bag

val empty_pot : unit -> pot

(**[Check_wager] takes in a choice [opt] and the current minimum bet
   [current_bet] that a player can make. It checks to see if the wager is a 
   valid one, returning true if it is valid and false if it is not valid. *)
val check_wager : choice -> int -> bool

val current_wager : choice -> int

(** [wager] places the amount [amt] from a player's bag [b1] into
    the pot [p1].
    Precondition: 
    [amt] is an int >= 0 and <= !b1 *)
val wager : choice -> pot -> bag -> int -> int -> unit

val max_wager : choice -> int -> bool

val clear : pot -> unit