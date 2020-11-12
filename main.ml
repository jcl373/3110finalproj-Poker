open Graphics

exception InvalidResponse

exception Empty

let gametable = Table.empty_table 5 10
let gamedeck = Deck.create
let max_wager = ref 0
let dealer_index = ref 0
let max_name_len = 12

let draw_box str =
  set_color black;
  fill_rect 200 350 320 20;
  set_color white;
  moveto 205 355;
  draw_string ("> " ^ str);
  if String.length str = max_name_len then begin moveto 455 355; set_color red; draw_string "Max length" end

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

let name_input unit = 
  let rec text_input str : string =
    draw_box str;
    let stat = wait_next_event (Key_pressed :: []) in
    if stat.key = '\027' || stat.key = '\r' then str 
    else if stat.key = '\b' then text_input (String.sub str 0 (String.length str - 1))
    else if (String.length str >= max_name_len) then text_input str
    else text_input (str ^ (Char.escaped stat.key))
  in
  text_input ""

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

let rec find_in_list lst x acc =
  match lst with
  | [] -> None
  | h :: t -> if h = x then Some acc else find_in_list t x (acc + 1)

let find_list lst x =
  find_in_list lst x 0

let extract_value = function
  | Some x -> x
  | None -> raise Empty;;


(* (Prompt.request_choice max_wager gametable round) *)
(* (Prompt.request_choice max_wager gametable round) *)
let choices round  = 
  if round = 1 then begin 
    print_string "round1";
    let startpos = (!dealer_index + 3) mod (List.length gametable.in_players) in
    gametable.last_bet <- n_of_list gametable.players startpos;
    iter_index startpos (Prompt.request_choice max_wager gametable round) gametable.players;
    if extract_value (find_list gametable.players (extract_value gametable.last_bet)) = 
       startpos then ()
    else 
      gametable.last_call <- 1; (* Need to only let Check / Call when last_call = 1 *)
    shorten_to_p (iter_index_snd startpos (extract_value gametable.last_bet) 
                    gametable.players) (extract_value gametable.last_bet) 
      (Prompt.request_choice max_wager gametable round) [] ; 
    gametable.last_call <- 0; end
  else begin
    print_string "round2";
    let startpos = (!dealer_index - (List.length gametable.out_players) + 1) mod (List.length gametable.in_players) in
    gametable.last_bet <- n_of_list gametable.players startpos;
    let inplayers = gametable.in_players in
    iter_index startpos (Prompt.request_choice max_wager gametable round) inplayers;
    if extract_value (find_list gametable.players (extract_value gametable.last_bet)) = 
       startpos then ()
    else 
      gametable.last_call <- 1; (* Need to only let Check / Call when last_call = 1 *)
    shorten_to_p (iter_index_snd startpos (extract_value gametable.last_bet) 
                    inplayers) (extract_value gametable.last_bet) 
      (Prompt.request_choice max_wager gametable round) [];
    gametable.last_call <- 0
  end
(*[create_bot] creates a bot with a name [name] and gives it 
  a [start_amt] number of chips *)
let create_bot name start_amt loc =
  Table.add_player gametable (Table.new_player name (Deck.pop gamedeck) (Deck.pop gamedeck) start_amt loc)

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

let draw_dealer (p : Table.person) =
  Prompt.draw_player p;
  set_color (rgb 200 200 200);
  fill_rect (fst (p.location) - 40) (snd (p.location) + 25) 80 15;
  moveto (fst (p.location) - 35) (snd (p.location) + 25);
  set_color black;
  draw_string "Dealer"

let draw_winner (p : Table.person) =
  Prompt.draw_player p;
  set_color yellow;
  fill_rect (fst (p.location) - 40) (snd (p.location) + 25) 80 15;
  moveto (fst (p.location) - 35) (snd (p.location) + 25);
  set_color black;
  draw_string "Winner"

let rec draw_players (players : Table.person list) (i : int) =
  match players with
  | [] -> ()
  | h :: t -> Prompt.draw_player h; draw_players t (i+1)

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
    draw_rect 0 0 750 750;
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
    Unix.sleepf 2.;

    (* Blind bets *)
    let sb = Table.extract_value (Table.n_of_list gametable.players ((!dealer_index + 1) mod List.length gametable.players)) in
    print_endline (sb.name ^ " has put forth a small blind of " ^ string_of_int (fst gametable.blinds) ^ " chips.");
    Bet.wager (Bet (fst gametable.blinds)) gametable.pot sb.chips (fst gametable.blinds) !max_wager;
    Prompt.draw_player sb; set_color yellow; moveto ((fst sb.location)-35) ((snd sb.location)-20); draw_string ("Blind bet " ^ string_of_int (fst gametable.blinds));
    Prompt.draw_pot gametable;
    Unix.sleepf 2.;

    let bb = Table.extract_value (Table.n_of_list gametable.players ((!dealer_index + 2) mod List.length gametable.players)) in
    print_endline (bb.name ^ " has put forth a big blind of " ^ string_of_int (snd gametable.blinds) ^ " chips.");
    Bet.wager (Bet (snd gametable.blinds)) gametable.pot (Table.extract_value (Table.n_of_list gametable.players ((!dealer_index + 2) mod List.length gametable.players))).chips (snd gametable.blinds) !max_wager;
    Prompt.draw_player bb; set_color yellow; moveto ((fst bb.location)-35) ((snd bb.location)-20); draw_string ("Blind bet " ^ string_of_int (snd gametable.blinds));
    Prompt.draw_pot gametable;
    Unix.sleepf 2.;
    max_wager := snd gametable.blinds;
    gametable.last_bet <- Some bb;

    (* Request choices *)
    choices 1 ;
    Unix.sleepf 2.;
    Table.last_one_wins gametable gamedeck round i;
    Table.side_pots_prep gametable 1;

    (* flop *)
    Table.init_commcard gametable gamedeck; 
    ANSITerminal.(print_string [green] ("The community cards are the " ^ print_card_list gametable.river ^ " The pot is " ^ string_of_int !(gametable.pot) ^ ".\n"));
    max_wager := 0;
    draw_table_cards gametable;
    draw_players gametable.players 0;


    (* Request choices *)
    choices 2 ;
    Unix.sleepf 2.;
    Table.last_one_wins gametable gamedeck round i;
    Table.side_pots_prep gametable 2;

    (* turn *)
    Table.add_commcard gametable gamedeck;
    ANSITerminal.(print_string [green] ("The community cards are the " ^ print_card_list gametable.river ^ " The pot is " ^ string_of_int !(gametable.pot) ^ ".\n"));
    max_wager := 0;
    draw_table_cards gametable;
    draw_players gametable.players 0;

    (* Request choices *)
    choices 3 ;
    Unix.sleepf 2.;
    Table.last_one_wins gametable gamedeck round i;
    Table.side_pots_prep gametable 3;

    (* river *)
    Table.add_commcard gametable gamedeck; 
    ANSITerminal.(print_string [green] ("The community cards are the " ^ print_card_list gametable.river ^ " The pot is " ^ string_of_int !(gametable.pot) ^ ".\n"));
    max_wager := 0;
    draw_table_cards gametable;
    draw_players gametable.players 0;

    (* Also goes to zero, max_wager. Shouldnt have effect. *)

    (* set winner *)
    let winner : Table.person = Game.evaluate_table gametable in
    draw_winner winner;
    Table.winner winner gametable gamedeck round i;

    (* TODO: side pots; raise max = allin; gui for invalid choices; gui to mark who is going; pot not preserving between rounds*)
  in
  round 0

(** [main ()] starts the game *)
let main () =
  (* FOR WINDOWS USERS *)
  (* open_graph "localhost:0.0 720x720";  *)

  (* FOR MAC USERS *)
  try begin 
    open_graph " 720x720";

    ANSITerminal.(print_string [red]
                    "\n\nWelcome to the poker game.\n");
    print_endline "Please enter your name.\n";
    print_string  "> ";

    set_color (rgb 200 200 200);
    fill_rect 200 350 320 55;
    set_color black;
    moveto 205 (355+35);
    draw_string "Welcome to the poker game.";
    moveto 205 (355+20);
    draw_string "Please enter your name.";
    start_game (name_input ())
  end
  with 
  | End_of_file -> print_newline ();
    print_endline "The user exited the game.";
    exit 0
  | Graphics.Graphic_failure x -> print_newline ();
    print_endline "The user closed the window.";
    exit 0
(*match read_line () with
  | exception End_of_file -> ()
  | name -> start_game name*)

(* Execute the game engine. *)
let () = main ()