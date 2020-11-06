type pos = 
  | Dealer
  | BB
  | LB
  | Folded
  | Leave
  | AllIn of int

exception Empty

exception Invalid_player

exception InvalidResponse

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

let rec print_card_tup tup : string =
  match tup with
  | (x,y) -> " the " ^ Deck.print_card x ^ " and the " ^ Deck.print_card y ^ "."

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

type table = {mutable pot : Bet.pot; 
              blinds: int * int; 
              mutable river: Deck.card list; 
              mutable players : person list; 
              mutable in_players : person list;
              mutable out_players : person list;
              mutable dealer : person option; 
              mutable round_num : int; 
              mutable side_pots : (int * person list) list;
             }


let new_player nm c1 c2 start_amt =
  {name = nm ; hand = (c1, c2); chips = Bet.add (Bet.empty_bag ()) start_amt;
   position = None}

let empty_table small_blind big_blind = 
  {pot = (Bet.empty_pot ()); blinds = (small_blind, big_blind); 
   river = []; players = []; in_players = []; out_players = []; 
   dealer = None; round_num = 1; side_pots = []} 

let set_hand (p : person) c1 c2 : unit =
  p.hand <- (c1, c2)

(** add_player adds a new player to the table. 
    [table] is a valid table
    [player] is a valid player *)
let add_player table player =
  table.players <- player :: table.players 

(** remove_player removes a player from the table. 
    [table] is a valid table
    [player] is a valid person *)
let remove_player table player =
  let players_list = table.players in 
  let updated_players = List.filter (fun x -> x <> player) players_list in 
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
  table.dealer <- Some dealer;
  table.in_players <- table.players

let next_br_prep table  = 
  let no_folds = List.filter (fun x -> x.position <> Some Folded) 
      table.in_players in table.in_players <- no_folds;
  let folded = List.filter (fun x -> x.position = Some Folded) 
      table.in_players in table.out_players <- folded

let compare_bags p1 p2 =
  if !(p1.chips) > !(p2.chips) then -1
  else if !(p1.chips) < !(p2.chips) then 1
  else 0

(*wIP*)

let rec updt_players table list amt =
  match list with 
  | [] -> ()
  | h :: t -> h.chips <- !(h.chips) - amt;


;;

(*wIP*)

let rec create_side_pots table list =
  let ct_players_in = List.length table.in_players in 
  match list with
  | [] ->  ()
  | h :: t -> 
    let sp = !(h.chips)*ct_players_in in
    table.side_pots <- 
      (sp, list) :: table.side_pots;
    table.pot <- !(table.pot) - sp;

;;


(*wIP*)
let side_pots_prep table round =
  let allin = List.filter (fun x -> x.position = Some (AllIn round)) 
      table.in_players in 
  let allin_dec = List.sort compare_bags allin in 
  table.side_pots <- (!(table.pot), allin) :: table.side_pots;
  table.pot <- Bet.empty_pot ()


(* let match_pos table x = 
   match x.position with
   | Some Dealer | Some BB | Some LB | Some Folded -> None
   | Some Leave -> remove_player table x
   | None -> None *)

(** Extract common functionality  *) 
let next_round_prep table =
  let players = List.map (fun x -> x.position <- None; x) table.players in 
  table.players <- players; table.in_players <- players; table.out_players <- []; 
  let curr_dealer = find_list table.players (extract_value table.dealer) in
  let curr_deal_int = if curr_dealer != None then extract_value curr_dealer else 0 in
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
  table.round_num <- table.round_num + 1

let auto_remove table (p : person)  : unit =
  if !(p.chips) < 10 then begin 
    print_endline (p.name ^ " has left because they ran out of chips.");
    remove_player table p
  end else () 

let rec end_prompt x f i  = 
  print_endline "Do you want to stay? (Yes or No)";
  print_endline "If you are out of chips, please restart game. ";
  print_string "> ";
  try 
    match read_line () with
    | "No" | "no" -> print_endline "Thanks for playing!"; exit 0
    | "Yes" | "yes" -> f (i+1)
    | _ -> raise (InvalidResponse)
  with 
  | InvalidResponse -> print_endline "Please type 'yes' or 'no'!"; 
    end_prompt 1 f i

let min_players gametable f i = 
  if List.length gametable.players <= 2 then begin
    print_endline "There are not enough players to continue. The game is over.";
    (* maybe add something about how much money i had and how much the max person had?*)
    exit 0
  end
  else end_prompt 1 f i



let winner winner gametable gamedeck f i= 
  (* let winner = win_list |> h_of_list |> extract_value |> fst in  *)

  ANSITerminal.(print_string [yellow] ("The winner is " ^ winner.name ^ ".\n" ^ "The winning hand is")); 
  ANSITerminal.(print_string [yellow] (print_card_tup winner.hand ^ "\n"));  (* TODO : make it say what their hand is *)
  winner.chips := !(winner.chips) + !(gametable.pot);
  gametable.pot := 0;

  (* new round *)
  gamedeck := !Deck.create;
  List.iter (auto_remove gametable) gametable.players;

  let rec reset_hand list =
    match list with
    | [] -> ()
    | h :: t -> set_hand h (Deck.pop gamedeck) (Deck.pop gamedeck); reset_hand t in
  min_players gametable f i;
  end_prompt 1 f i;
  reset_hand gametable.players

(*wIP*)
let rec elig_pots gametable player acc=
  let sidepots = gametable.side_pots in
  match sidepots with
  | [] -> acc
  | h :: t -> if List.exists (fun x -> x = player) (snd h) 
    then elig_pots gametable player (fst h + acc)
    else elig_pots gametable player acc

;;

(* FInishing this(*wIP*)
   let winning_player win_list gametable gamedeck f i =
   let win_p = win_list |> h_of_list |> extract_value |> fst in
   match win_p.position with
   | Some (AllIn x) -> elig_pots gametable win_p 0
   | _ -> win_p
   (* | _ -> winner win_p gametable gamedeck f i  *)
*)


let last_one_wins table gamedeck round i=
  if List.length table.in_players = 1 then 
    let def_win = extract_value (h_of_list table.in_players) in
    ANSITerminal.(print_string [yellow] ("Everyone folded except for " ^ def_win.name ^ ".\n")); 
    winner def_win table gamedeck round i
  else ()

