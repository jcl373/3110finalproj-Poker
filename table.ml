type pos = 
  | Dealer
  | BB
  | LB
  | Folded
  | Leave

exception Empty

exception Invalid_player

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

let rec find_in_list lst x acc =
  match lst with
  | [] -> None
  | h :: t -> if h = x then Some acc else find_in_list t x (acc + 1)

let find_list lst x =
  find_in_list lst x 0

let extract_value = function
  | Some x -> x
  | None -> raise Empty;;

(** The type [player] represents a player in the game. A player
    has a name, which is an identifier for the player, a hand, which is a pair of
    cards, and chips, which is the amount of money they have. *)
type person = {name : string; 
               mutable hand : Deck.card * Deck.card; 
               chips : Bet.bag; 
               mutable position : pos option
              } 

type table = {pot : Bet.pot; 
              blinds: int * int; 
              mutable river: Deck.card list; 
              mutable players : person list; 
              mutable out_players : person list;
              mutable dealer : person option; 
              mutable round_num : int
             }

let new_player nm c1 c2 start_amt =
  {name = nm ; hand = (c1, c2); chips = Bet.add (Bet.empty_bag ()) start_amt;
   position = None}

let empty_table small_blind big_blind = 
  {pot = (Bet.empty_pot ()); blinds = (small_blind, big_blind); 
   river = []; players = []; out_players = []; dealer = None; round_num = 1} 

let set_hand (p : person) c1 c2 : unit =
  p.hand <- (c1, c2)

(** add_player adds a new player to the table. 
    [table] is a valid table
    [player] is a valid player *)
let add_player table player =
  table.players <- player :: table.players 

(** remove_player removes a player from the table. 
    [table] is a valid table
    [player_name] is a valid player's name *)
let remove_player table player_name =
  let players_list = table.players in 
  let updated_players = List.filter (fun x -> x.name <> player_name ) players_list in 
  table.players <- updated_players

let init_commcard table deck =
  table.river <- Deck.pop deck :: table.river;
  table.river <- Deck.pop deck :: table.river;
  table.river <- Deck.pop deck :: table.river

(*Adds card to the community cards *)
let add_commcard table deck = 
  table.river <- Deck.pop deck :: table.river

(* let deal_start = table.players |> List.length |> Random.int
   maybe chain some of the stuff below, i was gonna make this change but then realized
   you need num_in_players later *) 
let choose_dealer table = 
  Random.self_init ();
  let num_in_players = List.length table.players in
  let deal_start = Random.int num_in_players in
  let dealer = extract_value (n_of_list table.players deal_start) in
  dealer.position <- Some Dealer; 
  let lb_start =  (deal_start + 1) mod num_in_players in
  let littleblinds = extract_value (n_of_list table.players lb_start) in
  littleblinds.position <- Some LB; 
  let bb_start =  (lb_start + 1) mod num_in_players in
  let bigblinds = extract_value (n_of_list table.players bb_start) in
  bigblinds.position <- Some BB;
  table.dealer <- Some dealer

let next_br_prep table = 
  let no_folds = List.filter (fun x -> x.position != Some Folded) 
      table.players in table.players <- no_folds;
  let folded = List.filter (fun x -> x.position = Some Folded) 
      table.players in table.out_players <- folded

(* let match_pos table x = 
   match x.position with
   | Some Dealer | Some BB | Some LB | Some Folded -> None
   | Some Leave -> remove_player table x
   | None -> None *)

(** Extract common functionality  *) 
let next_round_prep table =
  let players = List.map (fun x -> x.position <- None; x) table.players in 
  table.players <- players; table.out_players <- []; 
  let curr_dealer = find_list table.players (extract_value table.dealer) in
  let curr_deal_int = extract_value curr_dealer in
  let length = List.length table.players in
  let new_dealer =  n_of_list table.players ((curr_deal_int + 1) mod length) in 
  table.dealer <- new_dealer;  
  let lb_start =  (curr_deal_int + 2) mod length in
  let littleblinds = extract_value (n_of_list table.players lb_start) in
  littleblinds.position <- Some LB; 
  let bb_start =  (curr_deal_int + 3) mod length in
  let bigblinds = extract_value (n_of_list table.players bb_start) in
  bigblinds.position <- Some BB; 
  table.river <- [];
  Bet.clear table.pot; 
  table.round_num <- table.round_num + 1;