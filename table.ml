open Graphics

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

let win_sf = "The winner is "

let win_ss = ".\n" ^ "The winning hand is"

(** [nth_of_list] returns the nth element of the list [lst]
    Returns an option as the list may not contain that number 
    [lst] is a valid list
    [n] is an int; represents the nth element
    [acc] is an int; the accumulator *)
let rec nth_of_list lst n acc =  (* REMOVE, replace w librayr*)
  match lst with
  | [] -> None
  | h :: t -> if acc = n then Some h else nth_of_list t n (acc+1)

(** [n_of_list] returns the first (head) element of the list [lst]
    Returns an option as the list may not contain anything.
    [lst] is a valid list*)
let h_of_list lst = (* REMOVE, replace w librayr*)
  nth_of_list lst 0 0

(** [n_of_list] returns the nth element of the list [lst]
    Returns an option as the list may not contain that number.
    [lst] is a valid list
    [n] is an int; represents the nth element *)
let n_of_list lst n = (* REMOVE, replace w librayr*)
  nth_of_list lst n 0

let rec find_in_list lst x acc = (* REMOVE, replace w librayr*)
  match lst with
  | [] -> None
  | h :: t -> if h = x then Some acc else find_in_list t x (acc + 1)

let find_list lst x = (* REMOVE, replace w librayr*)
  find_in_list lst x 0

let rec print_card_tup tup : string =
  match tup with
  | (x,y) -> " the " ^ Deck.print_card x ^ " and the " ^ Deck.print_card y ^ "."

let extract_value = function (* REMOVE, replace w librayr*)
  | Some x -> x
  | None -> raise Empty

(** The type [player] represents a player in the game. A player
    has a name, which is an identifier for the player, a hand, which is a pair of
    cards, and chips, which is the amount of money they have. *)
type person = {name : string; 
               mutable hand : Deck.card * Deck.card; 
               chips : Bet.bag; 
               mutable last_bet : int;
               mutable position : pos option;
               location : int * int} 

type table = {mutable pot : Bet.pot; 
              blinds: int * int; 
              mutable river: Deck.card list; 
              mutable players : person list; 
              mutable in_players : person list;
              mutable out_players : person list;
              mutable dealer : person option; 
              mutable round_num : int; 
              mutable side_pots : (int * person list) list;
              mutable last_bet : person option;
              mutable last_call : int}

let new_player nm card1 card2 start_amt loc =
  {name = nm ; 
   hand = (card1, card2); 
   chips = Bet.add (Bet.empty_bag ()) start_amt;
   last_bet = 0; 
   position = None; 
   location = loc}

let empty_table small_blind big_blind = 
  {pot = (Bet.empty_pot ()); 
   blinds = (small_blind, big_blind); 
   river = []; 
   players = []; 
   in_players = []; 
   out_players = []; 
   dealer = None; 
   round_num = 1; 
   side_pots = []; 
   last_bet =  None;
   last_call = 0;}

let set_hand (player : person) card1 card2 : unit =
  player.hand <- (card1, card2)

let remove_folded (list : person list) = 
  List.filter (fun x -> x.position <> Some Folded) list

let add_player table player =
  table.players <- player :: table.players 

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

(* let deal_start = table.players |> List.length |> Random.int *) 
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
  table.in_players <- remove_folded table.in_players;
  let folded = List.filter (fun x -> x.position = Some Folded) 
      table.in_players in table.out_players <- folded

let side_pots_prep table round =
  let allin = 
    table.in_players 
    |> List.filter (fun x -> x.position = Some (AllIn round)) in 
  table.side_pots <- (!(table.pot), allin) :: table.side_pots
(* table.pot <- Bet.empty_pot () *)

(* let match_pos table x = 
   match x.position with
   | Some Dealer | Some BB | Some LB | Some Folded -> None
   | Some Leave -> remove_player table x
   | None -> None *)

let next_round_prep table =
  let players = List.map (fun x -> x.position <- None; x) table.players in 
  table.players <- players; 
  table.in_players <- players; 
  table.out_players <- []; 
  let curr_dealer = find_list table.players (extract_value table.dealer) in
  let curr_deal_int = 
    if curr_dealer != None then extract_value curr_dealer else 0 in
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

let draw_quit (hover : bool) =
  if hover then set_color (rgb 180 0 0) else set_color (rgb 220 40 0);
  fill_rect 275 170 80 50;
  set_color white;
  moveto 280 205;
  draw_string "Quit"

let draw_stay (hover : bool) =
  if hover then set_color (rgb 67 131 14) else set_color (rgb 87 175 13);
  fill_rect 365 170 80 50;
  set_color white;
  moveto 370 205;
  draw_string "Stay"

let rec exit_hover x f i =
  auto_synchronize false;
  let stat = wait_next_event (Button_down :: Mouse_motion :: Poll :: []) in
  if stat.mouse_x > 275 && stat.mouse_x < 355 && 
     stat.mouse_y > 170 && stat.mouse_y < 220
  then begin 
    draw_quit true; 
    auto_synchronize true; 
    if stat.button then begin 
      Graphics.close_graph (); 
      exit 0 end 
    else exit_hover x f i end
  else if stat.mouse_x > 365 && stat.mouse_x < 445 && 
          stat.mouse_y > 170 && stat.mouse_y < 220
  then begin 
    draw_stay true; 
    auto_synchronize true; 
    if stat.button then f (i + 1) else exit_hover x f i end
  else begin 
    draw_quit false; 
    auto_synchronize true; 
    draw_stay false; 
    exit_hover x f i end

let auto_remove table (player : person)  : unit =
  if !(player.chips) < 10 then begin 
    print_endline (player.name ^ " has left because they ran out of chips.");
    remove_player table player end 
  else () 

let end_prompt x f i  = 
  draw_quit false; 
  draw_stay false;
  exit_hover x f i

let min_players gametable f i = 
  if List.length gametable.players <= 2 then begin
    print_endline "There are not enough players to continue. The game is over.";
    exit 0
  end
  else end_prompt 1 f i

let winner winner gametable gamedeck f i = 
  ANSITerminal.(print_string [yellow] (win_sf ^ winner.name ^ win_ss)); 
  ANSITerminal.(print_string [yellow] (print_card_tup winner.hand ^ "\n"));
  winner.chips := !(winner.chips) + !(gametable.pot);
  gametable.pot := 0;
  gamedeck := !Deck.create;  (* new round *)
  List.iter (auto_remove gametable) gametable.players;
  gametable.in_players <- gametable.players;
  gametable.out_players <- [];
  let rec reset_hand lst =
    match lst with
    | [] -> ()
    | h :: t -> set_hand h (Deck.pop gamedeck) (Deck.pop gamedeck); 
      reset_hand t in
  min_players gametable f i;
  end_prompt 1 f i;
  reset_hand gametable.players

let rec elig_pots gametable player acc =
  let sidepots = gametable.side_pots in
  match sidepots with
  | [] -> acc
  | h :: t -> if List.exists (fun x -> x = player) (snd h) then 
      elig_pots gametable player (fst h + acc)
    else elig_pots gametable player acc

(* Finishing this *)
let winning_player win_list gametable gamedeck f i =
  let win_p = win_list |> h_of_list |> extract_value |> fst in
  match win_p.position with
  | Some (AllIn x) -> elig_pots gametable win_p 0
  | _ -> failwith "TODO"
(* | _ -> winner win_p gametable gamedeck f i  *)

let last_one_wins table gamedeck round i =
  if List.length table.in_players = 1 then 
    let def_win = extract_value (h_of_list table.in_players) in
    ("Everyone folded except for " ^ def_win.name ^ ".\n") |>
    ANSITerminal.(print_string [yellow]); 
    Some def_win
  else None