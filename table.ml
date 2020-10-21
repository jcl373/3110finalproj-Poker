type pos = 
  | Dealer
  | BB
  | LB
  | Folded

exception Empty

(** [nth_of_list] returns the nth element of the list [lst]
    Returns an option as the list may not contain that number 
    [lst] is a valid list
    [n] is an int; represents the nth element
    [acc] is an int; the accumulator *)
let rec nth_of_list lst n acc = 
  match lst with
  | [] -> None
  | h :: t -> if acc = n then Some h else nth_of_list t n (acc+1)

(** [n_of_list] returns the first (head) element of the list [lst]
    Returns an option as the list may not contain anything.
    [lst] is a valid list*)
let h_of_list lst =
  nth_of_list lst 0 0

(** [n_of_list] returns the nth element of the list [lst]
    Returns an option as the list may not contain that number.
    [lst] is a valid list
    [n] is an int; represents the nth element *)
let n_of_list lst n =
  nth_of_list lst n 0

let extract_value = function
  | Some x -> x
  | None -> raise Empty ;;

(** The type [player] represents a player in the game. A player
    has a name, which is an identifier for the player, a hand, which is a pair of
    cards, and chips, which is the amount of money they have. *)
type person = {name : string; mutable hand: Deck.card * Deck.card; 
               chips : Bet.bag; mutable position : pos option } 

type table = {pot : Bet.pot ; blinds: int* int; mutable river: Deck.card list; mutable players : person list}

let round_num = 0

let new_player nm c1 c2 start_amt =
  {name = nm ; hand = (c1, c2); chips = Bet.add (Bet.empty_bag ()) start_amt;
   position = None}

let empty_table small_blind big_blind = 
  {pot = (Bet.empty_pot ()); blinds = (small_blind, big_blind); river = []; players = []} 


(** add_player adds a new player to the table. 
    [table] is a valid table
    [player] is a valid player *)

let add_player table player =
  table.players <- player :: table.players 

(** remove_player removes a player from the table. 
    [table] is a valid table
    [player] is a valid player *)
let remove_player table player =
  failwith ("unimplemented")


let choose_dealer table = 
  let num_players = List.length table.players in
  let deal_start = Random.int num_players in
  let dealer = extract_value (n_of_list table.players deal_start) in
  dealer.position <- Some Dealer; 
  let bb_start =  (deal_start + 1) mod num_players in
  let bigblinds = extract_value (n_of_list table.players bb_start) in
  bigblinds.position <- Some BB; 
  let lb_start =  (bb_start + 1) mod num_players in
  let littleblinds = extract_value (n_of_list table.players lb_start) in
  littleblinds.position <- Some LB; 

  let next_round_prep table =
    failwith ("unimplemented")