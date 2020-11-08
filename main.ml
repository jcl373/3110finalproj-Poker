open Graphics

exception InvalidResponse

let gametable = Table.empty_table 5 10
let gamedeck = Deck.create
let max_wager = ref 0
let dealer_index = ref 0

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
  | h :: t -> if (i = 0) then List.iter f list else iter_index (i-1) f (t @ (h :: []))

let choices round = 
  if round = 1 then 
    let startpos = (!dealer_index + 3) mod (List.length gametable.in_players) in
    iter_index startpos (Prompt.request_choice max_wager gametable round) gametable.players
  else 
    let startpos = (!dealer_index - (List.length gametable.out_players) + 1) mod (List.length gametable.in_players) in
    iter_index startpos (Prompt.request_choice max_wager gametable round) gametable.in_players

(*[create_bot] creates a bot with a name [name] and gives it 
  a [start_amt] number of chips *)
let create_bot name start_amt =
  Table.add_player gametable (Table.new_player name (Deck.pop gamedeck) (Deck.pop gamedeck) start_amt)

let six_locations = [|(360,250);(175,275);(175,445);(360,470);(545,445);(545,275)|]

let draw_table () =
  set_color (rgb 68 125 35);
  fill_rect 235 235 250 250;
  fill_circle 235 360 125;
  fill_circle 485 360 125

let draw_card (c : Deck.card) (x : int) (y : int) =
  set_color white;
  fill_rect x y 38 60;
  if c.suit = 'C' || c.suit = 'S' then set_color black else set_color red;
  moveto (x + 5) (y + 45);
  (match c.rank with
   | 1 -> draw_string "A"
   | 11 -> draw_string "J"
   | 12 -> draw_string "Q"
   | 13 -> draw_string "K"
   | n -> draw_string (string_of_int n));
  moveto (x + 5) (y + 15);
  draw_string (Char.escaped c.suit)

let draw_table_cards (t : Table.table) =
  let rec cards (c : Deck.card list) (i : int) =
    match c with
    | [] -> ()
    | h :: t -> begin 
        draw_card h (245 + i * 48) 330;
        cards t (i+1);
      end in
  cards t.river 0

let draw_player_cards (p : Table.person) =
  set_color white;
  draw_card (fst p.hand) 409 240;
  draw_card (snd p.hand) (409 + 48) 240

let draw_player (x : int) (y : int) (p : Table.person) =
  set_color (rgb 70 70 70);
  fill_rect (x-40) (y-25) 80 50;
  moveto (x-35) (y+10);
  set_color white;
  draw_string p.name;
  moveto (x-35) (y-5);
  draw_string (string_of_int !(p.chips))

let rec draw_players (players : Table.person list) (i : int) =
  match players with
  | [] -> ()
  | h :: t -> draw_player (fst six_locations.(i)) (snd six_locations.(i)) h; draw_players t (i+1)

let start_game name =
  (* Fill table with 5 bots + the player *)
  create_bot "Bot 5" 100;
  create_bot "Bot 4" 100;
  create_bot "Bot 3" 100;
  create_bot "Bot 2" 100;
  create_bot "Bot 1" 100; 
  let player = (Table.new_player name (Deck.pop gamedeck) (Deck.pop gamedeck) 100) in
  Table.add_player gametable player;

  draw_players gametable.players 0;

  (* Pick random dealer *)
  Table.choose_dealer gametable;

  let rec round i =
    (* Print cards in hole *)
    print_endline ("Your cards are the " ^ Deck.print_card (fst player.hand) ^ " and the " ^ Deck.print_card (snd player.hand) ^ ".");
    draw_player_cards (player);

    (* Dealer / advance round *)    
    if i = 0 then () 
    else Table.next_round_prep gametable;
    dealer_index := Table.extract_value(Table.find_list gametable.players (Table.extract_value gametable.dealer));
    print_endline ("The current dealer is " ^ (Table.extract_value gametable.dealer).name);

    (* Blind bets *)
    print_endline ((Table.extract_value (Table.n_of_list gametable.players ((!dealer_index + 1) mod List.length gametable.players))).name ^ " has put forth a small blind of " ^ string_of_int (fst gametable.blinds) ^ " chips.");
    print_endline ((Table.extract_value (Table.n_of_list gametable.players ((!dealer_index + 2) mod List.length gametable.players))).name ^ " has put forth a big blind of " ^ string_of_int (snd gametable.blinds) ^ " chips.");
    Bet.wager (Bet (fst gametable.blinds)) gametable.pot (Table.extract_value (Table.n_of_list gametable.players ((!dealer_index + 1) mod List.length gametable.players))).chips (fst gametable.blinds) !max_wager;
    Bet.wager (Bet (snd gametable.blinds)) gametable.pot (Table.extract_value (Table.n_of_list gametable.players ((!dealer_index + 2) mod List.length gametable.players))).chips (snd gametable.blinds) !max_wager;
    max_wager := snd gametable.blinds;

    (* Request choices *)
    choices 1;
    Table.last_one_wins gametable gamedeck round i;
    Table.side_pots_prep gametable 1;

    (* flop *)
    Table.init_commcard gametable gamedeck; 
    ANSITerminal.(print_string [green] ("The community cards are the " ^ print_card_list gametable.river ^ " The pot is " ^ string_of_int !(gametable.pot) ^ ".\n"));
    max_wager := 0;
    draw_table_cards gametable;

    (* Request choices *)
    choices 2;

    Table.last_one_wins gametable gamedeck round i;
    Table.side_pots_prep gametable 2;

    (* turn *)
    Table.add_commcard gametable gamedeck;
    ANSITerminal.(print_string [green] ("The community cards are the " ^ print_card_list gametable.river ^ " The pot is " ^ string_of_int !(gametable.pot) ^ ".\n"));
    max_wager := 0;
    draw_table_cards gametable;

    (* Request choices *)
    choices 3;
    Table.last_one_wins gametable gamedeck round i;
    Table.side_pots_prep gametable 3;

    (* river *)
    Table.add_commcard gametable gamedeck; 
    ANSITerminal.(print_string [green] ("The community cards are the " ^ print_card_list gametable.river ^ " The pot is " ^ string_of_int !(gametable.pot) ^ ".\n"));
    max_wager := 0;
    draw_table_cards gametable;

    (* Also goes to zero, max_wager. Shouldnt have effect. *)

    (* set winner *)
    let winner : Table.person = Game.evaluate_table gametable in
    Table.winner winner gametable gamedeck round i;
  in
  round 0

(** [main ()] starts the game *)
let main () =
  open_graph "localhost:0.0 720x720";
  draw_table ();

  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the poker game.\n");
  print_endline "Please enter your name.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | name -> start_game name

(* Execute the game engine. *)
let () = main ()