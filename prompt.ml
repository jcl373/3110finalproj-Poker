
exception InvalidResponse

let bot_choice (p : Table.person) max_wager: Bet.choice =
  Random.float (Unix.gettimeofday ()); (* What is the purpose of this*)
  match Random.int 4 with 
  | 999 -> Check
  | 0 -> Fold
  | 1 -> Bet 11 (* TODO : fix AI *)
  | 2 -> Call !max_wager
  | 3 -> Raise 11 (* TODO : fix AI *)
  | 999 -> AllIn !(p.chips)
  | _ -> failwith "impossible"


let print_choice (c : Bet.choice) (p : Table.person) : unit =
  match c with
  | Check -> print_endline (p.name ^ " has checked.")
  | Fold -> print_endline (p.name ^ " has folded.")
  | Bet i -> print_endline (p.name ^ " has bet " ^ string_of_int i ^ " chips.")
  | Call i -> print_endline (p.name ^ " has called.");
  | Raise i -> print_endline (p.name ^ " has raised " ^ string_of_int i ^ " chips.")
  | AllIn i -> print_endline (p.name ^ " has gone all in.")

let parse str (p : Table.person) max_wager : Bet.choice =
  let lst = String.split_on_char ' ' str in
  let lst = List.filter ((<>) "") lst in
  match lst with
  | [] -> raise(Bet.InvalidResponse) 
  | h :: t -> if h = "Check" then Check
    else if h = "Fold" then Fold
    else if h = "Bet" then Bet (int_of_string (String.concat "" t))
    else if h = "Call" then Call !max_wager
    else if h = "Raise" then Raise (int_of_string (String.concat "" t))
    else if h = "AllIn" || h = "Allin" then AllIn !(p.chips)
    else raise(Bet.InvalidResponse) 

let rec request_choice  max_wager (gametable : Table.table) (p : Table.person) : unit =
  if (p.name = "Bot 1" || p.name = "Bot 2" || p.name = "Bot 3" || p.name = "Bot 4" || p.name = "Bot 5")
  then let bot_bet = bot_choice p max_wager in
    if Bet.current_wager bot_bet > !max_wager then max_wager := Bet.current_wager bot_bet;
    Bet.wager bot_bet gametable.pot p.chips (Bet.current_wager bot_bet) !max_wager;
    print_choice bot_bet p
  else begin
    try 
      print_endline "What is your choice?";
      print_string "Choose between: 'Fold', 'Bet x', 'Call', 'Raise x', or 'AllIn' ";
      print_endline "where x is an amount that you would like to bet.";
      print_string "> ";
      let input = read_line () in 
      let player_bet = parse input p max_wager in  
      if Bet.current_wager player_bet > !max_wager then max_wager := Bet.current_wager player_bet;
      Bet.wager player_bet gametable.pot p.chips (Bet.current_wager player_bet) !max_wager;
      print_choice player_bet p
    with 
    | Bet.InvalidResponse -> print_endline "Invalid Choice. Try again.";
      request_choice max_wager gametable p
  end
