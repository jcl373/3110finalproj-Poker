open Graphics

exception InvalidResponse

let bot_choice (p : Table.person) max_wager : Bet.choice =
  Random.self_init ();
  match Random.int 3 with 
  | 0 -> if !max_wager = 0 && !(p.chips) > 5 then Bet 5
    else if !max_wager < !(p.chips) then Call !max_wager
    else Fold
  | 1 -> if !max_wager = 0 && !(p.chips) > 5 then Bet 5
    else if !max_wager + 5 < !(p.chips) then Raise (!max_wager + 5) 
    else Fold
  | 2 -> if !max_wager = 0 && !(p.chips) > 5 then Bet 5 
    else if !(p.chips) > !max_wager then AllIn !(p.chips) 
    else Fold
  | _ -> failwith "impossible"

let string_of_choice (c : Bet.choice) (p : Table.person) : string =
  match c with
  | Check -> set_color yellow; "Check"
  | Fold -> set_color red; "Fold"
  | Bet i -> set_color green; "Bet " ^ string_of_int i
  | Call i -> set_color green; "Call " ^ string_of_int i
  | Raise i -> set_color green; "Raise " ^ string_of_int i
  | AllIn i -> set_color green; "All In"

let print_choice (c : Bet.choice) (p : Table.person) : unit =
  match c with
  | Check -> print_endline (p.name ^ " has checked.")
  | Fold -> print_endline (p.name ^ " has folded.")
  | Bet i -> print_endline (p.name ^ " has bet " ^ string_of_int i ^ " chips.")
  | Call i -> print_endline (p.name ^ " has called and bet " ^ string_of_int i ^ " chips.")
  | Raise i -> print_endline (p.name ^ " has raised " ^ string_of_int i ^ " chips.")
  | AllIn i -> print_endline (p.name ^ " has gone all in and bet " ^ string_of_int i ^ " chips.")

let parse str (p : Table.person) max_wager : Bet.choice =
  let lst = String.split_on_char ' ' str in
  let lst = List.filter ((<>) "") lst in
  match lst with
  | [] -> raise(Bet.InvalidResponse) 
  | h :: t -> 
    begin 
      if !max_wager = 0 then 
        begin 
          if h = "Check" then Check
          else if h = "Fold" then Fold
          else if h = "Bet" then Bet (int_of_string (String.concat "" t))
          else if h = "Raise" then Raise (int_of_string (String.concat "" t))
          else if h = "AllIn" || h = "Allin" then AllIn !(p.chips)
          else raise(Bet.InvalidResponse) 
        end
      else 
      if h = "Check" then Check
      else if h = "Fold" then Fold
      else if h = "Call" then Call !max_wager
      else if h = "Raise" then Raise (int_of_string (String.concat "" t))
      else if h = "AllIn" || h = "Allin" then AllIn !(p.chips)
      else raise(Bet.InvalidResponse) 
    end 

let draw_player (p : Table.person) =
  let x = fst p.location in
  let y = snd p.location in
  set_color (rgb 70 70 70);
  fill_rect (x-40) (y-25) 80 50;
  moveto (x-35) (y+10);
  set_color white;
  draw_string p.name;
  moveto (x-35) (y-5);
  draw_string (string_of_int !(p.chips))

let draw_pot (t : Table.table) : unit =
  set_color (rgb 68 125 35);
  fill_rect 300 425 100 15;
  set_color white;
  moveto 300 425;
  draw_string ("Total pot: " ^ (string_of_int !(t.pot)))

let rec request_choice max_wager (gametable : Table.table) round (p : Table.person) : unit =
  if List.length gametable.in_players = 1 then () else
  if (p.name = "Bot 1" || p.name = "Bot 2" || p.name = "Bot 3" || p.name = "Bot 4" || p.name = "Bot 5")
  then let bot_bet = bot_choice p max_wager in
    match bot_bet with 
    | Fold -> p.position <- Some Folded; print_choice bot_bet p; Unix.sleepf 2.; draw_player p; draw_pot gametable; moveto ((fst p.location)-35) ((snd p.location)-20); draw_string (string_of_choice bot_bet p); Table.next_br_prep gametable 
    | AllIn x -> p.position <- Some (AllIn round); print_choice bot_bet p; Unix.sleepf 2.; draw_player p; draw_pot gametable; moveto ((fst p.location)-35) ((snd p.location)-20); draw_string (string_of_choice bot_bet p);
    | _ -> 
      if Bet.current_wager bot_bet > !max_wager then max_wager := Bet.current_wager bot_bet;
      Bet.wager bot_bet gametable.pot p.chips (Bet.current_wager bot_bet) !max_wager;
      Unix.sleepf 2.;
      draw_player p; draw_pot gametable; moveto ((fst p.location)-35) ((snd p.location)-20); draw_string (string_of_choice bot_bet p);
      print_choice bot_bet p
  else 
    match p.position with 
    | Some Folded   -> ()
    | Some (AllIn _) -> ()
    | _ ->
      begin
        try
          begin 
            if !max_wager = 0 then 
              begin 
                print_endline "What is your choice?";
                print_endline ("Current wager is: " ^ string_of_int !max_wager ^".");
                print_endline ("You have "^ string_of_int !(p.chips) ^ " chips.");
                print_string "Choose between: 'Fold', 'Check', 'Bet x', or 'AllIn' ";
                print_endline "where x is an amount that you would like to bet.";
                print_string "> ";
              end
            else 
              begin
                print_endline "What is your choice?";
                print_endline ("Current wager is: " ^ string_of_int !max_wager ^".");
                print_endline ("You have "^ string_of_int !(p.chips) ^ " chips.");
                print_string "Choose between: 'Fold', 'Call', 'Raise x', or 'AllIn' ";
                print_endline "where x is an amount that you would like to bet.";
                print_string "> ";
              end;
            let input = read_line () in 
            let player_bet = parse input p max_wager in  
            let bet_check = Bet.check_wager player_bet !max_wager in
            match player_bet with
            | Fold -> p.position <- Some Folded; draw_player p; draw_pot gametable; moveto ((fst p.location)-35) ((snd p.location)-20); draw_string (string_of_choice player_bet p); Table.next_br_prep gametable 
            | AllIn x -> p.position <- Some (AllIn round); draw_player p; draw_pot gametable; moveto ((fst p.location)-35) ((snd p.location)-20); draw_string (string_of_choice player_bet p);
            | _ ->
              if bet_check = false || Bet.max_wager player_bet !(p.chips) = false
              then raise(Bet.InvalidWager) 
              else if (Bet.current_wager player_bet > !max_wager) && (Bet.current_wager player_bet <= !(p.chips))
              then max_wager := Bet.current_wager player_bet;
              Bet.wager player_bet gametable.pot p.chips (Bet.current_wager player_bet) !max_wager;
              draw_player p; draw_pot gametable; moveto ((fst p.location)-35) ((snd p.location)-20); draw_string (string_of_choice player_bet p);
              print_choice player_bet p
          end
        with 
        | Bet.InvalidResponse -> print_endline "Invalid Choice. Try again.";
          request_choice max_wager gametable round p 
        | Bet.InvalidWager -> print_endline "Invalid Wager Amount. Try again.";
          request_choice max_wager gametable round p 
      end



