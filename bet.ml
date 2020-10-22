

type bag = int ref

type pot = int ref

exception InvalidWager

type choice = 
  | Check
  | Fold
  | Bet of int 
  | Call of int
  | Raise of int
  | AllIn of int (*Maybe change this later to just AllIn *)

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

(** [current_wager] takes in a choice [opt] and returns the amount that 
    the player is contributing to the pot. 
    [opt] is a valid choice *)
let current_wager (opt : choice) = 
  match opt with
  |Check -> 0
  |Fold -> 0
  |Bet x -> x
  |Call x -> x
  |Raise x -> x
  |AllIn x -> x

(**[Check_wager] takes in a choice [opt] and the current minimum bet
   [current_bet] that a player can make. It checks to see if the wager is a 
   valid one, returning true if it is valid and false if it is not valid. *)
let check_wager (opt: choice) (current_bet: int) =
  match opt with
  |Check -> true
  |Fold -> true
  |Bet x -> if x > current_bet then false else true
  |Call x -> if x > current_bet then false else true
  |Raise x -> if x <= current_bet then false else true
  |AllIn x -> true


(** [wager] places the amount [amt] from a player's bag [b1] into
    the pot [p1].
    Precondition: 
    [p1] is a valid pot
    [b1] is a valid bag
    [amt] is an int >= 0 and <= !b1 *)
let wager (opt : choice) (p1 : pot) (b1 : bag) amt (current_bet: int) =  
  if (current_wager opt > !b1) && not (check_wager opt current_bet) then raise InvalidWager else
    p1 := !p1 + amt; 
  b1 := !b1 - amt

let clear pot = 
  pot := 0

(** [win_pot] takes in a player and a pot, and transfers the pot amount
    into the players chips. It then clears the pot*)
let win_pot (player : Table.person) (p1 : pot) =
  player.chips := !(player.chips) + !p1;
  clear p1


