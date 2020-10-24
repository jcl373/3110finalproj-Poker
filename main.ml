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

let rec iter_index (i : int) (f : 'a -> unit) (list : 'a list) : unit =
  match list with
  | [] -> failwith "empty iter list"
  | h :: t -> if (i = 0) then List.iter f list else iter_index (i-1) f (t @ (h :: []))

let choices round = 
  if round = 1 then 
    let startpos = (!dealer_index + 3) mod (List.length gametable.players) in
    iter_index startpos (Prompt.request_choice max_wager gametable) gametable.players
  else 
    let startpos = (!dealer_index + 1) mod (List.length gametable.players) in
    iter_index startpos (Prompt.request_choice max_wager gametable) gametable.players

(*[create_bot] creates a bot with a name [name] and gives it 
  a [start_amt] number of chips *)
let create_bot name start_amt =
  Table.add_player gametable (Table.new_player name (Deck.pop gamedeck) (Deck.pop gamedeck) start_amt)

let start_game name =
  (* Fill table with 5 bots + the player *)
  create_bot "Bot 5" 100;
  create_bot "Bot 4" 100;
  create_bot "Bot 3" 100;
  create_bot "Bot 2" 100;
  create_bot "Bot 1" 100; 
  let player = (Table.new_player name (Deck.pop gamedeck) (Deck.pop gamedeck) 100) in
  Table.add_player gametable player;

  (* Pick random dealer *)
  Table.choose_dealer gametable;

  let rec round i =
    (* Print cards in hole *)
    print_endline ("Your cards are the " ^ Deck.print_card (fst player.hand) ^ " and the " ^ Deck.print_card (snd player.hand) ^ ".");

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

    (* flop *)
    Table.init_commcard gametable gamedeck; 
    ANSITerminal.(print_string [green] ("The community cards are the " ^ print_card_list gametable.river ^ " The pot is " ^ string_of_int !(gametable.pot) ^ "\n"));
    max_wager := 0;

    (* Request choices *)
    choices 2;

    (* turn *)
    Table.add_commcard gametable gamedeck;
    ANSITerminal.(print_string [green] ("The community cards are the " ^ print_card_list gametable.river ^ " The pot is " ^ string_of_int !(gametable.pot) ^ "\n"));
    max_wager := 0;

    (* Request choices *)
    choices 3;

    (* river *)
    Table.add_commcard gametable gamedeck; 
    ANSITerminal.(print_string [green] ("The community cards are the " ^ print_card_list gametable.river ^ " The pot is " ^ string_of_int !(gametable.pot) ^ "\n"));
    max_wager := 0;
    (* Also goes to zero, max_wager. Shouldnt have effect. *)

    (* set winner *)
    let winner : Table.person = Game.evaluate_table gametable in
    ANSITerminal.(print_string [yellow] ("The winner is " ^ winner.name ^ ".\n")); (* TODO : make it say what their hand is *)
    winner.chips := !(winner.chips) + !(gametable.pot);
    gametable.pot := 0;

    (* new round *)
    gamedeck := !Deck.create;
    let remove (p : Table.person) : unit =
      if !(p.chips) < 10 then begin 
        print_endline (p.name ^ " has left because they ran out of chips.");
        Table.remove_player gametable p
      end else () in
    List.iter remove gametable.players;

    let rec reset_hand list =
      match list with
      | [] -> ()
      | h :: t -> Table.set_hand h (Deck.pop gamedeck) (Deck.pop gamedeck); reset_hand t in

    let rec end_prompt x = 
      print_endline "Do you want to stay? (Yes or No)";
      print_string "> ";
      try 
        match read_line () with
        | "No" -> print_endline "Thanks for playing!"
        | "Yes" -> round (i+1)
        | _ -> raise (InvalidResponse)
      with 
      | InvalidResponse -> print_endline "Please type yes or no!"; 
        end_prompt 1
    in
    reset_hand gametable.players;
    if List.length gametable.players < 2 then begin
      print_endline "There are not enough players to continue. The game is over.";
      ()
    end
    else end_prompt 1
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