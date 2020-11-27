type bag = int ref

type pot = int ref

exception InvalidWager

exception InvalidResponse

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

let set amt b1 =
  b1 := (amt - !b1)

(** [current_wager] takes in a choice [opt] and returns the amount that 
    the player is contributing to the pot. 
    [opt] is a valid choice *)
let current_wager (opt : choice) = 
  match opt with
  | Check -> 0
  | Fold -> 0
  | Bet x -> x
  | Call x -> x
  | Raise x -> x
  | AllIn x -> x

let check_wager (opt: choice) (current_bet: int) =
  match opt with
  | Check -> true
  | Fold -> true
  | Bet x | Call x -> if x >= current_bet then true else false
  | Raise x -> if x <= current_bet || current_bet = 0 then false else true
  | AllIn x -> true

let max_wager (opt: choice) (p : int) = 
  match opt with
  | Bet x | Call x | Raise x -> if x > p then false else true
  | _ -> true

(** [wager] places the amount [amt] from a player's bag [b1] into
    the pot [p1].
    Precondition: 
    [p1] is a valid pot
    [b1] is a valid bag
    [amt] is an int >= 0 and <= !b1 *)
let wager (opt : choice) (p1 : pot) (b1 : bag) amt (current_bet: int) =  
  if (current_wager opt > !b1) && not (check_wager opt current_bet) then 
    raise InvalidWager else
    p1 := !p1 + amt; 
  b1 := !b1 - amt

let clear pot = 
  pot := 0