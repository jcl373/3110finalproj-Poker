let gametable = Table.empty_table 5 10
let gamedeck = Deck.create
let max_wager = ref 0
let dealer_index = ref 0

let bot_choice (p : Table.person): Bet.choice =
  match Random.init (int_of_float (Unix.gettimeofday ())); Random.int 4 with 
  | 999 -> Check
  | 0 -> Fold
  | 1 -> Bet 11 (* TODO : fix AI *)
  | 2 -> Call !max_wager
  | 3 -> Raise 11 (* TODO : fix AI *)
  | 999 -> AllIn !(p.chips)
  | _ -> failwith "impossible"

let parse str (p : Table.person) : Bet.choice =
  let lst = String.split_on_char ' ' str in
  let lst = List.filter ((<>) "") lst in
  match lst with
  | [] -> failwith "no response"
  | h :: t -> if h = "Check" then Check
    else if h = "Fold" then Fold
    else if h = "Bet" then Bet (int_of_string (String.concat "" t))
    else if h = "Call" then Call !max_wager
    else if h = "Raise" then Raise (int_of_string (String.concat "" t))
    else if h = "AllIn" then AllIn !(p.chips)
    else failwith "invalid choice"

let print_choice (c : Bet.choice) (p : Table.person) : unit =
  match c with
  | Check -> print_endline (p.name ^ " has checked.")
  | Fold -> print_endline (p.name ^ " has folded.")
  | Bet i -> print_endline (p.name ^ " has bet " ^ string_of_int i ^ " chips.")
  | Call i -> print_endline (p.name ^ " has called.");
  | Raise i -> print_endline (p.name ^ " has raised " ^ string_of_int i ^ " chips.")
  | AllIn i -> print_endline (p.name ^ " has gone all in.")

let request_choice (p : Table.person) : unit =
  if (p.name = "Bot 1" || p.name = "Bot 2" || p.name = "Bot 3" || p.name = "Bot 4" || p.name = "Bot 5")
  then let bot_bet = bot_choice p in
    if Bet.current_wager bot_bet > !max_wager then max_wager := Bet.current_wager bot_bet;
    Bet.wager bot_bet gametable.pot p.chips (Bet.current_wager bot_bet) !max_wager;
    print_choice bot_bet p
  else begin
    print_endline "What is your choice?";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> failwith "error"
    | response -> let player_bet = (parse response p) in
      if Bet.current_wager player_bet > !max_wager then max_wager := Bet.current_wager player_bet;
      Bet.wager player_bet gametable.pot p.chips (Bet.current_wager player_bet) !max_wager;
      print_choice player_bet p
  end

let rec print_card_list list : string =
  match list with
  | [] -> failwith "can't print empty deck"
  | h :: [] -> "and the " ^ Deck.print_card h ^ "."
  | h :: t -> "the " ^ Deck.print_card h ^ ", " ^ (print_card_list t)

let rec iter_index (i : int) (f : 'a -> unit) (list : 'a list) : unit =
  match list with
  | [] -> failwith "empty iter list"
  | h :: t -> if (i = 0) then List.iter f list else iter_index (i-1) f (t @ (h :: []))

let start_game name =
  (* Fill table with 5 bots + the player *)
  Table.add_player gametable (Table.new_player "Bot 5" (Deck.pop gamedeck) (Deck.pop gamedeck) 100);
  Table.add_player gametable (Table.new_player "Bot 4" (Deck.pop gamedeck) (Deck.pop gamedeck) 100);
  Table.add_player gametable (Table.new_player "Bot 3" (Deck.pop gamedeck) (Deck.pop gamedeck) 100);
  Table.add_player gametable (Table.new_player "Bot 2" (Deck.pop gamedeck) (Deck.pop gamedeck) 100);
  Table.add_player gametable (Table.new_player "Bot 1" (Deck.pop gamedeck) (Deck.pop gamedeck) 100);
  let player = (Table.new_player name (Deck.pop gamedeck) (Deck.pop gamedeck) 100) in
  Table.add_player gametable player;

  (* Pick random dealer *)
  Table.choose_dealer gametable;

  let rec round i =
    (* Print cards in hole *)
    print_endline ("Your cards are the " ^ Deck.print_card (fst player.hand) ^ " and the " ^ Deck.print_card (snd player.hand) ^ ".");

    (* Dealer / advance round *)  
    Table.next_round_prep gametable;
    dealer_index := Table.extract_value(Table.find_list gametable.players (Table.extract_value gametable.dealer));
    print_endline ("The current dealer is " ^ (Table.extract_value gametable.dealer).name);

    (* Blind bets *)
    print_endline ((Table.extract_value (Table.n_of_list gametable.players ((!dealer_index + 1) mod (List.length gametable.players)))).name ^ " has put forth a small blind of " ^ string_of_int (fst gametable.blinds) ^ " chips.");
    print_endline ((Table.extract_value (Table.n_of_list gametable.players ((!dealer_index + 2) mod (List.length gametable.players)))).name ^ " has put forth a big blind of " ^ string_of_int (snd gametable.blinds) ^ " chips.");
    Bet.wager (Bet (fst gametable.blinds)) gametable.pot (Table.extract_value (Table.n_of_list gametable.players ((!dealer_index + 1) mod (List.length gametable.players)))).chips (fst gametable.blinds) !max_wager;
    Bet.wager (Bet (snd gametable.blinds)) gametable.pot (Table.extract_value (Table.n_of_list gametable.players ((!dealer_index + 2) mod (List.length gametable.players)))).chips (fst gametable.blinds) !max_wager;
    max_wager := snd gametable.blinds;

    (* Request choices *)
    (* TODO : add conditions so folding/betting/checking actually works *)
    iter_index ((!dealer_index + 3) mod (List.length gametable.players)) request_choice gametable.players;

    (* flop *)
    gametable.river <- Deck.pop gamedeck :: gametable.river;
    gametable.river <- Deck.pop gamedeck :: gametable.river;
    gametable.river <- Deck.pop gamedeck :: gametable.river;
    print_endline ("The community cards are the " ^ print_card_list gametable.river);
    max_wager := 0;

    (* Request choices *)
    iter_index ((!dealer_index + 1) mod (List.length gametable.players)) request_choice gametable.players;

    (* turn *)
    gametable.river <- Deck.pop gamedeck :: gametable.river;
    print_endline ("The community cards are the " ^ print_card_list gametable.river);
    max_wager := 0;

    (* Request choices *)
    iter_index ((!dealer_index + 1) mod (List.length gametable.players)) request_choice gametable.players;

    (* river *)
    gametable.river <- Deck.pop gamedeck :: gametable.river;
    print_endline ("The community cards are the " ^ print_card_list gametable.river);

    (* set winner *)
    let winner : Table.person = Game.evaluate_table gametable in
    print_endline ("The winner is " ^ winner.name ^ "."); (* TODO : make it say what their hand is *)
    winner.chips := !(winner.chips) + !(gametable.pot);
    gametable.pot := 0;

    (* new round *)
    gamedeck := !Deck.create;
    let rec reset_hand list =
      match list with
      | [] -> ()
      | h :: t -> Table.set_hand h (Deck.pop gamedeck) (Deck.pop gamedeck); reset_hand t in
    reset_hand gametable.players;
    print_endline "Do you want to stay? (Yes or No)";
    print_string "> ";
    match read_line () with
    | "No" -> ()
    | "Yes" -> round (i+1)
    | _ -> failwith "invalid response"
  in
  round 0

(** [main ()] starts the game *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the poker game.\n");
  print_endline "Please enter your name.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | name -> start_game name

(* Execute the game engine. *)
let () = main ()