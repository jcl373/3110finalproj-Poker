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

let rec find_in_list lst x acc = 
  match lst with
  | [] -> None
  | h :: t -> if h = x then Some acc else find_in_list t x (acc + 1)

let find_list lst x = 
  find_in_list lst x 0

let rec print_card_tup tup : string =
  match tup with
  | (x, y) -> " " ^ Deck.print_card x ^ " and " ^ Deck.print_card y ^ "."

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
              blinds : int * int; 
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
  let dealer = Option.get (List.nth_opt table.players deal_start) in
  dealer.position <- Some Dealer; 

  let lb_start =  (deal_start + 1) mod num_in_players in
  (Option.get (List.nth_opt table.players lb_start)).position <- Some LB; 
  let bb_start =  (lb_start + 1) mod num_in_players in
  (Option.get (List.nth_opt table.players bb_start)).position <- Some BB;

  table.dealer <- Some dealer;
  table.in_players <- table.players

let next_br_prep table  = 
  let folded = List.filter (fun x -> x.position = Some Folded) table.in_players
  in table.in_players <- remove_folded table.in_players;
  table.out_players <- folded

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
  let length = List.length table.players in

  let curr_dealer = find_list table.players (Option.get table.dealer) in
  let curr_deal_int = if curr_dealer != None then Option.get curr_dealer 
    else 0 in
  table.dealer <- (List.nth_opt table.players ((curr_deal_int + 1) mod length)); 

  let lb_start =  (curr_deal_int + 2) mod length in
  (Option.get (List.nth_opt table.players lb_start)).position <- Some LB; 
  let bb_start =  (curr_deal_int + 3) mod length in
  (Option.get (List.nth_opt table.players bb_start)).position <- Some BB; 

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
     stat.mouse_y > 170 && stat.mouse_y < 220 then begin 
    draw_quit true; 
    auto_synchronize true; 
    if stat.button then begin 
      Graphics.close_graph (); 
      exit 0 end 
    else exit_hover x f i end
  else if stat.mouse_x > 365 && stat.mouse_x < 445 && 
          stat.mouse_y > 170 && stat.mouse_y < 220 then begin 
    draw_stay true; 
    auto_synchronize true; 
    if stat.button then f (i + 1) else exit_hover x f i end
  else begin 
    draw_quit false; 
    auto_synchronize true; 
    draw_stay false; 
    exit_hover x f i end

let draw_ok_end (hover : bool) =
  if hover then set_color (rgb 180 0 0) else set_color (rgb 220 40 0);
  fill_rect 275 170 170 50;
  set_color white;
  moveto 280 205;
  draw_string "The game is over.";
  moveto 280 193;
  draw_string "There are not enough ";
  moveto 280 181;
  draw_string "players to continue.";
  moveto 280 169;
  draw_string "Click to quit."

let rec exit_hover_min x f i =
  auto_synchronize false;
  let stat = wait_next_event (Button_down :: Mouse_motion :: Poll :: []) in
  if stat.mouse_x > 275 && stat.mouse_x < 445 && 
     stat.mouse_y > 170 && stat.mouse_y < 220 then begin 
    draw_ok_end true; 
    auto_synchronize true; 
    if stat.button then begin 
      Graphics.close_graph (); 
      exit 0 end 
    else exit_hover_min x f i end
  else begin 
    draw_ok_end false; 
    auto_synchronize true; 
    draw_ok_end false; 
    exit_hover_min x f i end



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
    draw_ok_end false;
    exit_hover_min 1 f i
  end
  else end_prompt 1 f i

let rec reset_pos (players : person list) (i : int) =
  match players with
  | [] -> ()
  | h :: t -> begin 
      if h.position = Some Folded 
      then h.position <- None; reset_pos t (i+1) end


let rec reset_hand lst gamedeck =
  match lst with
  | [] -> ()
  | h :: t -> set_hand h (Deck.pop gamedeck) (Deck.pop gamedeck); 
    reset_hand t gamedeck


let winner winner gametable gamedeck f i = 
  ANSITerminal.(print_string [yellow] (win_sf ^ winner.name ^ win_ss)); 
  ANSITerminal.(print_string [yellow] (print_card_tup winner.hand ^ "\n"));
  winner.chips := !(winner.chips) + !(gametable.pot);
  gametable.pot := 0;
  gamedeck := !(Deck.create ()); (* new round *)
  List.iter (auto_remove gametable) gametable.players;
  gametable.in_players <- gametable.players;
  gametable.out_players <- [];
  reset_pos gametable.players 0;
  reset_hand gametable.players gamedeck;
  min_players gametable f i;
  end_prompt 1 f i



let rec elig_pots gametable player acc =
  let sidepots = gametable.side_pots in
  match sidepots with
  | [] -> acc
  | h :: t -> if List.exists (fun x -> x = player) (snd h) 
    then elig_pots gametable player (fst h + acc)
    else elig_pots gametable player acc

(* Finishing this *)
let winning_player win_list gametable gamedeck f i =
  let winner = List.nth_opt win_list 0 |> Option.get |> fst in
  match winner.position with
  | Some (AllIn x) -> elig_pots gametable winner 0
  | _ -> failwith "TODO"
(* | _ -> winner win_player gametable gamedeck f i  *)

let last_one_wins table gamedeck round i =
  if List.length table.in_players = 1 then 
    let definite_win = List.nth_opt table.in_players 0 |> Option.get in
    ("Everyone folded except for " ^ definite_win.name ^ ".\n") |>
    ANSITerminal.(print_string [yellow]); 
    Some definite_win
  else None