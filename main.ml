open Graphics
open Gui

exception InvalidResponse

exception Empty

let gametable = Table.empty_table 5 10
let gamedeck = Deck.create ()
let max_wager = ref 0
let dealer_index = ref 0
let max_name_len = 12

(* let draw_box str =
   set_color black;
   fill_rect 200 350 320 20;
   set_color white;
   moveto 205 355;
   draw_string ("> " ^ str);
   if String.length str = max_name_len then begin moveto 455 355; set_color red; 
   draw_string "Max length" end *)

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
  | [] -> failwith "Can't print empty deck"
  | h :: [] -> "and the " ^ Deck.print_card h ^ "."
  | h :: t -> "the " ^ Deck.print_card h ^ ", " ^ (print_card_list t)

let rec print_list (list : Table.person list) : string =
  match list with
  | [] -> ""
  | h :: t -> h.name^" " ^ print_list t

let rec iter_index (i : int) (f : 'a -> unit) (list : 'a list) : unit =
  match list with
  | [] -> failwith "Can't iterate on empty list"
  | h :: t -> if (i = 0) then List.iter f list
    else iter_index (i-1) f (t @ (h :: []))

let rec iter_index_snd (i : int) p (list : 'a list)  =
  match list with
  | [] -> failwith "Can't iterate on empty list"
  | h :: t -> if (i = 0) then list else iter_index_snd (i-1) p (t @ (h :: []))

let rec shorten_to_p lst p f acc =
  match lst with 
  | [] -> failwith "Can't iterate on empty list"
  | h :: t -> if h = p then List.iter f (List.rev acc) 
    else shorten_to_p t p f (h :: acc)

let rec choices round  = 
  if round = 1 then begin 
    let startpos = 
      (List.length gametable.in_players)
      |> (mod) (!dealer_index + 3) in
    choices_helper startpos round gametable.players
  end
  else begin
    let startpos = 
      List.length gametable.in_players 
      |> (mod) (!dealer_index - (List.length gametable.out_players) + 1) in
    let inplayers = gametable.in_players in
    (* print_endline("inplayers");
       print_int(  List.length gametable.in_players );
       print_endline("outplayers");
       print_int(  List.length gametable.out_players );
       print_endline("val");
       print_int( !dealer_index - (List.length gametable.out_players) + 1  ); *)
    (* gametable.last_bet <- Table.n_of_list gametable.players startpos; *)
    choices_helper startpos round inplayers
  end

and choices_helper startpos round inplayers =
  gametable.last_bet <- List.nth_opt gametable.players startpos;
  (* let inplayers = gametable.in_players in *)
  inplayers |>
  iter_index startpos (Prompt.request_choice max_wager gametable round);
  let last_bet = Option.get gametable.last_bet in
  print_endline("inplayers");
  print_int(List.length inplayers);
  print_endline("startpos");
  print_int(  startpos );
  print_endline(last_bet.name);
  print_int( Option.get (Table.find_list gametable.players (last_bet)));


  if Option.get (Table.find_list gametable.players (last_bet)) = 
     startpos then ()
  else begin
    gametable.last_call <- 1;
    shorten_to_p (iter_index_snd startpos last_bet inplayers) last_bet
      (Prompt.request_choice max_wager gametable round) [];
    gametable.last_call <- 0 end

(*[create_bot] creates a bot with a name [name] and gives it 
  a [start_amt] number of chips *)
let create_bot name start_amt loc =
  let card1 = Deck.pop gamedeck in
  let card2 = Deck.pop gamedeck in
  Table.add_player gametable (Table.new_player name card1 card2 start_amt loc)

let mid_winner round i = 
  let possible_winner = Table.last_one_wins gametable gamedeck round i in
  if possible_winner <> None then begin 
    let winner = Option.get possible_winner in 
    draw_winner winner;
    Table.winner winner gametable gamedeck round i end
  else
    Table.side_pots_prep gametable 1


let blinds (person : Table.person) f  = 
  Bet.wager (Bet (f gametable.blinds)) gametable.pot 
    person.chips (f gametable.blinds) !max_wager;
  person.last_bet <- f gametable.blinds;
  max_wager := f gametable.blinds;
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

let create_bots = 
  create_bot "Bot 5" 100 six_locations.(5);
  create_bot "Bot 4" 100 six_locations.(4);
  create_bot "Bot 3" 100 six_locations.(3);
  create_bot "Bot 2" 100 six_locations.(2);
  create_bot "Bot 1" 100 six_locations.(1)

let create_player name = 
  let card1 = Deck.pop gamedeck in
  let card2 = Deck.pop gamedeck in
  let player = Table.new_player name card1 card2 100 six_locations.(0) in
  Table.add_player gametable player;
  player

let start_game name =
  (* Fill table with 5 bots + the player *)
  create_bots;
  let player = create_player name in

  (* Pick random dealer *)
  Table.choose_dealer gametable;

  let rec round i =
    set_color white;
    fill_rect 0 0 750 750;
    draw_table ();
    draw_players gametable.players 0;

    (* Print cards in hole *)
    (* print_endline ("Your cards are the " ^ Deck.print_card (fst player.hand) ^ " and the " ^ Deck.print_card (snd player.hand) ^ "."); *)
    draw_player_cards (player);

    (* Dealer / advance round *)    
    if i = 0 then () 
    else Table.next_round_prep gametable;
    dealer_index := 
      (Option.get gametable.dealer) 
      |> Table.find_list gametable.players 
      |>  Option.get;
    draw_dealer (Option.get gametable.dealer);
    print_endline ("The current dealer is " ^ (Option.get gametable.dealer).name);
    Unix.sleepf 0.5;

    (* Blind bets *)
    let sb = Option.get (List.nth_opt gametable.players ((!dealer_index + 1) mod List.length gametable.players)) in
    print_endline (sb.name ^ " has put forth a small blind of " ^ string_of_int (fst gametable.blinds) ^ " chips.");
    blinds sb fst;

    let bb = Option.get (List.nth_opt gametable.players ((!dealer_index + 2) mod List.length gametable.players)) in
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
    draw_players gametable.players 0

  (* TODO: side pots; cleaner UI; last call prices *)
  in
  round 0

(** [main ()] starts the game *)
let main () =
  try begin 
    begin try   open_graph " 720x720" with
      | Graphics.Graphic_failure _ -> open_graph "localhost:0.0 720x720" end;

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