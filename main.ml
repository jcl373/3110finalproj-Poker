(* open Graphics *)

open Gui

exception InvalidResponse

exception Empty

let gametable = Table.empty_table 5 10
let gamedeck = Deck.create
let max_wager = ref 0
let dealer_index = ref 0
let max_name_len = 12

(* let draw_box str =
   set_color black;
   fill_rect 200 350 320 20;
   set_color white;
   moveto 205 355;
   draw_string ("> " ^ str);
   if String.length str = max_name_len then begin moveto 455 355; set_color red; draw_string "Max length" end *)


(* let name_input unit = 
   let rec text_input str : string =
    draw_box str;
    let stat = wait_next_event (Key_pressed :: []) in
    if stat.key = '\027' || stat.key = '\r' then str 
    else if stat.key = '\b' then text_input (String.sub str 0 (String.length str - 1))
    else if (String.length str >= max_name_len) then text_input str
    else text_input (str ^ (Char.escaped stat.key))
   in
   text_input "" *)

let rec print_card_list list : string =
  match list with
  | [] -> failwith "can't print empty deck"
  | h :: [] -> "and the " ^ Deck.print_card h ^ "."
  | h :: t -> "the " ^ Deck.print_card h ^ ", " ^ (print_card_list t)

let rec print_list (list : Table.person list) : string =
  match list with
  | [] -> ""
  | h :: t -> h.name^" " ^ print_list t

let rec iter_index (i : int) (f : 'a -> unit) (list : 'a list) : unit =
  match list with
  | [] -> failwith "empty iter list"
  | h :: t -> if (i = 0) then List.iter f list
    else iter_index (i-1) f (t @ (h :: []))

let rec iter_index_snd (i : int) p (list : 'a list)  =
  match list with
  | [] -> failwith "empty iter list"
  | h :: t -> if (i = 0) then list else iter_index_snd (i-1) p (t @ (h :: []))

let rec shorten_to_p lst p f acc =
  match lst with 
  | [] -> failwith "empty iter list"
  | h :: t -> if h = p then List.iter f (List.rev acc) 
    else shorten_to_p t p f (h :: acc)


let choices round  = 
  if round = 1 then begin 
    let startpos = (!dealer_index + 3) mod (List.length gametable.in_players) in
    gametable.last_bet <- Table.n_of_list gametable.players startpos;
    iter_index startpos (Prompt.request_choice max_wager gametable round) gametable.players;
    if Table.extract_value (Table.find_list gametable.players (Table.extract_value gametable.last_bet)) = 
       startpos then ()
    else 
      gametable.last_call <- 1; (* Need to only let Check / Call when last_call = 1 *)
    shorten_to_p (iter_index_snd startpos (Table.extract_value gametable.last_bet) 
                    gametable.players) (Table.extract_value gametable.last_bet) 
      (Prompt.request_choice max_wager gametable round) []; 
    gametable.last_call <- 0; end
  else begin
    let startpos = (!dealer_index - (List.length gametable.out_players) + 1) mod (List.length gametable.in_players) in
    gametable.last_bet <- Table.n_of_list gametable.players startpos;
    let inplayers = gametable.in_players in
    iter_index startpos (Prompt.request_choice max_wager gametable round) inplayers;
    if Table.extract_value (Table.find_list gametable.players (Table.extract_value gametable.last_bet)) = 
       startpos then ()
    else 
      gametable.last_call <- 1; (* Need to only let Check / Call when last_call = 1 *)
    shorten_to_p (iter_index_snd startpos (Table.extract_value gametable.last_bet) 
                    inplayers) (Table.extract_value gametable.last_bet) 
      (Prompt.request_choice max_wager gametable round) [];
    gametable.last_call <- 0
  end

(*[create_bot] creates a bot with a name [name] and gives it 
  a [start_amt] number of chips *)
let create_bot name start_amt loc =
  Table.add_player gametable (Table.new_player name (Deck.pop gamedeck) (Deck.pop gamedeck) start_amt loc)

let mid_winner round i = 
  let possible_winner = Table.last_one_wins gametable gamedeck round i in
  if possible_winner <> None then begin 
    let winner = Table.extract_value possible_winner in 
    draw_winner winner;
    Table.winner winner gametable gamedeck round i end
  else
    Table.side_pots_prep gametable 1


let blinds (person : Table.person) f  = 
  Bet.wager (Bet (f gametable.blinds)) gametable.pot 
    person.chips (f gametable.blinds) !max_wager;
  draw_blinds person gametable f

let step n round i =
  choices n;
  Unix.sleepf 2.;
  mid_winner round i;
  if n = 1 then 
    Table.init_commcard gametable gamedeck
  else 
    Table.add_commcard gametable gamedeck;
  ("The community cards are the " ^ print_card_list gametable.river ^ 
   " The pot is " ^ string_of_int !(gametable.pot) ^ ".\n") |>  
  ANSITerminal.(print_string [green]) ;
  max_wager := 0;
  draw_table_cards gametable;
  draw_players gametable.players 0



let start_game name =
  (* Fill table with 5 bots + the player *)
  create_bot "Bot 5" 100 six_locations.(5);
  create_bot "Bot 4" 100 six_locations.(4);
  create_bot "Bot 3" 100 six_locations.(3);
  create_bot "Bot 2" 100 six_locations.(2);
  create_bot "Bot 1" 100 six_locations.(1); 
  let player = (Table.new_player name (Deck.pop gamedeck) (Deck.pop gamedeck) 100 six_locations.(0)) in
  Table.add_player gametable player;

  (* Pick random dealer *)
  Table.choose_dealer gametable;

  let rec round i =
    set_color white;
    fill_rect 0 0 750 750;
    draw_table ();
    draw_players gametable.players 0;

    (* Print cards in hole *)
    print_endline ("Your cards are the " ^ Deck.print_card (fst player.hand) ^ " and the " ^ Deck.print_card (snd player.hand) ^ ".");
    draw_player_cards (player);

    (* Dealer / advance round *)    
    if i = 0 then () 
    else Table.next_round_prep gametable;
    dealer_index := Table.extract_value(Table.find_list gametable.players (Table.extract_value gametable.dealer));
    draw_dealer (Table.extract_value gametable.dealer);
    print_endline ("The current dealer is " ^ (Table.extract_value gametable.dealer).name);
    Unix.sleepf 0.5;

    (* Blind bets *)
    let sb = Table.extract_value (Table.n_of_list gametable.players ((!dealer_index + 1) mod List.length gametable.players)) in
    print_endline (sb.name ^ " has put forth a small blind of " ^ string_of_int (fst gametable.blinds) ^ " chips.");
    blinds sb fst;

    let bb = Table.extract_value (Table.n_of_list gametable.players ((!dealer_index + 2) mod List.length gametable.players)) in
    print_endline (bb.name ^ " has put forth a big blind of " ^ string_of_int (snd gametable.blinds) ^ " chips.");
    blinds bb snd;
    max_wager := snd gametable.blinds;
    gametable.last_bet <- Some bb;


    step 1 round i;
    step 2 round i;
    step 3 round i;

    (* set winner *)
    let winner : Table.person = Game.evaluate_table gametable in
    draw_winner winner;
    Table.winner winner gametable gamedeck round i;

    (* TODO: side pots; raise max = allin; gui for invalid choices; pot not preserving between rounds*)
  in
  round 0

(** [main ()] starts the game *)
let main () =
  try begin 
    (* FOR WINDOWS USERS *)
    (*open_graph "localhost:0.0 720x720";*)
    (* FOR MAC USERS *)
    open_graph " 720x720";

    ANSITerminal.(print_string [red]
                    "\n\nWelcome to the poker game.\n");
    print_endline "Please enter your name.\n";
    print_string  "> ";
    set_color (rgb 200 200 200); fill_rect 200 350 320 55;
    set_color black; moveto 205 (355+35);
    draw_string "Welcome to the poker game."; moveto 205 (355+20);
    draw_string "Please enter your name."; start_game (name_input ()) end
  with 
  | End_of_file | Graphics.Graphic_failure _ -> print_newline ();
    print_endline "The user closed the window.";
    exit 0

(*match read_line () with
  | exception End_of_file -> ()
  | name -> start_game name*)

(* Execute the game engine. *)
let () = main ()