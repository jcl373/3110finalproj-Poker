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

(** [amount] returns the amount of money in a given bag. *)
val amount : bag -> int

val add : bag -> int -> bag 

val empty_bag : unit -> bag

val empty_pot : unit -> pot

val check_wager : choice -> int -> bool

val wager : choice -> pot -> bag -> int -> int -> unit

val win_pot : Table.person -> pot -> unit

val clear : pot -> unit



