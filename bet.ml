

type bag = int ref

type pot = int ref

exception InvalidWager

type choice = 
  | Check
  | Fold
  | Bet of int
  | Call of int
  | Raise of int
  | AllIn

(** [amount] returns the amount of money in a given bag [b1]. 
    [b1] is a valid bag.*)
let amount b1 = !b1

let empty_bag () = ref 0 

let empty_pot () = ref 0 

(** [add] adds the amount [amt] to the player's bag [b1].
    Precondition: 
    [b1] is a valid bag
    [amt] is an int >= 0 *)
let add b1 amt = 
  b1 := !b1 + amt;
  b1

(* let current_wager (opt: choice) = 
   match opt with
   |Check -> 0
   |Fold -> 0
   |Bet x -> x *) 

(** [wager] places the amount [amt] from a player's bag [b1] into
    the pot [p1].
    Precondition: 
    [p1] is a valid pot
    [b1] is a valid bag
    [amt] is an int >= 0 and <= !b1 *)
let wager (p1 : pot) (b1 : bag) amt =  
  p1 := !p1 + amt; 
  b1 := !b1-amt

let clear pot = 
  pot := 0

