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
  | AllIn of int

(** [amount bag] returns the amount of money in a given bag [bag]. 
    [bag] is a valid bag.*)
let amount bag = !bag

let empty_bag () = ref 0 

let empty_pot () = ref 0 

(** [add bag amt] adds the amount [amt] to the player's bag [bag].
    Precondition: 
    [bag] is a valid bag
    [amt] is an int >= 0 *)
let add bag amt = 
  bag := !bag + amt;
  bag

let set amt bag =
  bag := (amt - !bag)

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

let check_wager (opt : choice) (current_bet: int) =
  match opt with
  | Check -> true
  | Fold -> true
  | Bet x | Call x -> x >= current_bet
  | Raise x -> not (x <= current_bet || current_bet = 0)
  | AllIn x -> true

let max_wager (opt : choice) (max : int) = 
  match opt with
  | Bet x | Call x | Raise x -> x <= max
  | _ -> true

let wager (opt : choice) (pot : pot) (bag : bag) amt (current_bet : int) =  
  if current_wager opt > !bag && not (check_wager opt current_bet) then 
    raise InvalidWager
  else pot := !pot + amt; 
  bag := !bag - amt

let clear pot = 
  pot := 0