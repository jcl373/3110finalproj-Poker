open Graphics
open Gui

exception InvalidResponse
exception Empty

let gametable = Table.empty_table 5 10
let gamedeck = Deck.create ()
let max_wager = ref 0
let dealer_index = ref 0  

let rec print_card_list lst : string =
  match lst with
  | [] -> failwith "Can't print empty deck"
  | h :: [] -> "and the " ^ Deck.print_card h ^ "."
  | h :: t -> "the " ^ Deck.print_card h ^ ", " ^ (print_card_list t)

let rec print_list (lst : Table.person list) : string =
  match lst with
  | [] -> ""
  | h :: t -> h.name^" " ^ print_list t

let rec iter_index (i : int) (f : 'a -> unit) (lst : 'a list) : unit =
  match lst with
  | [] -> failwith "Can't iterate on empty list"
  | h :: t -> if (i = 0) then List.iter f lst
    else iter_index (i-1) f (t @ (h :: []))

let rec iter_index_snd (i : int) p (lst : 'a list)  =
  match lst with
  | [] -> failwith "Can't iterate on empty list"
  | h :: t -> if (i = 0) then lst else iter_index_snd (i-1) p (t @ (h :: []))

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
    choices_helper startpos round gametable.players 10
  end
  else begin
    let startpos = 
      List.length gametable.in_players 
      |> (mod) (!dealer_index - (List.length gametable.out_players) + 1) in
    choices_helper startpos round gametable.in_players 0
  end

and choices_helper startpos round in_players start =
  gametable.last_bet <- List.nth_opt gametable.players startpos;
  in_players |>
  iter_index startpos (Prompt.request_choice max_wager gametable round);
  let last_bet = Option.get gametable.last_bet in
  if Option.get (Table.find_list gametable.players (last_bet)) = 
     startpos && !max_wager = start then ()
  else begin
    gametable.last_call <- 1;
    shorten_to_p (iter_index_snd startpos last_bet in_players) last_bet
      (Prompt.request_choice max_wager gametable round) [];
    gametable.last_call <- 0 end

(* [create_bot] creates a bot with a name [name] and gives it 
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
  if n = 1 then Table.init_commcard gametable gamedeck
  else Table.add_commcard gametable gamedeck;
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

let round_gfx player =
  set_color white;
  fill_rect 0 0 750 750;

  draw_table ();
  draw_players gametable.players 0;
  draw_player_cards player

(* TODO : longer than 20 lines *)
let start_game name =
  create_bots;
  let player = create_player name in
  let num_players = List.length gametable.players in
  Table.choose_dealer gametable;

  let rec round i =
    round_gfx player;

    if i = 0 then () else Table.next_round_prep gametable;
    dealer_index := gametable.dealer
                    |> Option.get
                    |> Table.find_list gametable.players 
                    |> Option.get;
    draw_dealer (Option.get gametable.dealer);
    Unix.sleepf 0.5;

    let sb = Option.get (List.nth_opt gametable.players 
                           ((!dealer_index + 1) mod num_players)) in
    blinds sb fst;
    let bb = Option.get (List.nth_opt gametable.players 
                           ((!dealer_index + 2) mod num_players)) in
    blinds bb snd;
    max_wager := snd gametable.blinds;
    gametable.last_bet <- Some bb;

    step 1 round i;
    step 2 round i;
    step 3 round i;

    let winner : Table.person = Game.evaluate_table gametable in
    draw_winner winner;
    Table.winner winner gametable gamedeck round i;
    draw_players gametable.players 0 in
  round 0

(** [main ()] starts the game *)
let main () =
  try begin 
    begin try open_graph " 720x720" with
      | Graphics.Graphic_failure _ -> open_graph "localhost:0.0 720x720" end;

    set_color (rgb 200 200 200); 
    fill_rect 200 350 320 55;

    set_color black; 
    moveto 205 390;
    draw_string "Welcome to the poker game."; 
    moveto 205 375;
    draw_string "Please enter your name."; 
    start_game (name_input ()) end
  with 
  | End_of_file | Graphics.Graphic_failure _ -> print_newline ();
    print_endline "The user closed the window.";
    exit 0

(* Execute the game engine. *)
let () = main ()