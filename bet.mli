(** Handles betting and the pot. *)

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

(** Raised when an unknown response is encountered. *)
exception InvalidResponse

(** Raised when a wager is impossible or not allowed by the response. *)
exception InvalidWager

(** [amount b1] returns the amount of money in bag [b1]. *)
val amount : bag -> int

(** [add b1 amt] add the amount [amt] to bag [b1]. *)
val add : bag -> int -> bag 

(** [set amt b1] sets the amount of pot [b1] to [amt] - ![b1]. *)
val set : int -> bag -> unit

(** [empty_bag ()] returns a reference to a new empty bag. *)
val empty_bag : unit -> bag

(** [empty_pot ()] returns a reference to a new empty pot. *)
val empty_pot : unit -> pot

(** [check_wager opt current_bet] takes in a choice [opt] and the current 
    minimum bet [current_bet] that a player can make. It checks to see if the 
    wager is valid, returning true if it is valid and false if it is not 
    valid. *)
val check_wager : choice -> int -> bool

(** [current_wager opt] returns the amount that 
    the player is contributing to the pot from a valid choice [opt]. *)
val current_wager : choice -> int

(** [wager opt p1 b1 amt current_bet] places the amount [amt] from a player's bag [b1] into
    the pot [p1] with the current bet being [current_bet].
    Precondition: 
    [amt] is an int >= 0 and <= ![b1] *)
val wager : choice -> pot -> bag -> int -> int -> unit

(** [max_wager opt p] returns whether the response [opt] exceeds the 
    value [p]. *)
val max_wager : choice -> int -> bool

(** [clear pot] sets the value of [pot] to 0. *)
val clear : pot -> unit