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
  | 2 -> if !max_wager = 0 && !(p.chips) > 5 then Check
    else if !(p.chips) > !max_wager then Call (!max_wager ) 
    (* else if !(p.chips) > !max_wager then AllIn !(p.chips)  *)
    else Fold
  | _ -> failwith "impossible"

let bot_choice_fold (p : Table.person) max_wager : Bet.choice =
  if !max_wager < !(p.chips) then Call !max_wager 
  (* else if !(p.chips) > !max_wager then AllIn !(p.chips)  *)
  else Fold


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
  draw_string (string_of_int !(p.chips));
  match p.position with
  | Some Folded -> moveto ((fst p.location)-35) ((snd p.location)-20); set_color red; draw_string "Fold"; ()
  | Some AllIn x -> moveto ((fst p.location)-35) ((snd p.location)-20); set_color green; draw_string ("All in " ^ string_of_int x); ()
  | _ -> ()

let draw_pot (t : Table.table) : unit =
  set_color (rgb 68 125 35);
  fill_rect 300 425 100 15;
  set_color white;
  moveto 300 425;
  draw_string ("Total pot: " ^ (string_of_int !(t.pot)))

let player_bet_opt max_wager (gametable : Table.table) (p : Table.person) 
    (player_bet : Bet.choice) bet_check =
  if gametable.last_call = 0 then
    match player_bet with 
    | Raise x | Bet x | AllIn x -> gametable.last_bet <- Some p
    | _ -> ()
  else ();
  if bet_check = false || Bet.max_wager player_bet !(p.chips) = false
  then raise(Bet.InvalidWager) 
  else if (Bet.current_wager player_bet > !max_wager) && 
          (Bet.current_wager player_bet <= !(p.chips))
  then max_wager := Bet.current_wager player_bet;
  Bet.wager player_bet gametable.pot p.chips 
    (Bet.current_wager player_bet) !max_wager;
  draw_player p; draw_pot gametable; 
  moveto ((fst p.location)-35) ((snd p.location)-20); 
  draw_string (string_of_choice player_bet p);
  print_choice player_bet p

let bot_bet_opt max_wager (gametable : Table.table) (p : Table.person) 
    (bot_bet : Bet.choice) =
  if gametable.last_call = 0 then
    match bot_bet with 
    | Raise x | Bet x | AllIn x -> gametable.last_bet <- Some p
    | _ -> ()
  else ();
  if Bet.current_wager bot_bet > !max_wager 
  then max_wager := Bet.current_wager bot_bet;
  Bet.wager bot_bet gametable.pot p.chips (Bet.current_wager bot_bet) 
    !max_wager;
  Unix.sleepf 1.;
  draw_player p; draw_pot gametable; 
  moveto ((fst p.location)-35) ((snd p.location)-20); 
  draw_string (string_of_choice bot_bet p);
  print_choice bot_bet p

let draw_bet_raise (hover : bool) (bet : bool) =
  if hover then set_color (rgb 67 131 14) else set_color (rgb 87 175 13);
  fill_rect (360-40+80+5) (250-25-50-5) 80 50;
  set_color white;
  moveto (360-40+80+10) (250-25-15-5);
  if bet then draw_string "Bet" else draw_string "Raise"

let draw_call_check (hover : bool) (check : bool) (max_wager : int) =
  if hover then set_color (rgb 188 153 0) else set_color (rgb 255 208 0);
  fill_rect (360-40) (250-25-50-5) 80 50;
  set_color white;
  moveto (360-35) (250-25-15-5);
  if check then draw_string "Check" else begin draw_string "Call"; 
    moveto (360-35) (250-25-15-5-15); draw_string (string_of_int max_wager) end

let draw_fold (hover : bool) =
  if hover then set_color (rgb 180 0 0) else set_color (rgb 220 40 0);
  fill_rect (360-80-40-5) (250-25-50-5) 80 50;
  set_color white;
  moveto (360-80-40) (250-25-15-5);
  draw_string "Fold"

let erase_box unit =
  set_color white;
  fill_rect (360-80-40-5) (250-25-50-30) (80+80+5+5+80) 20

let erase_options unit =
  erase_box ();
  fill_rect (360-80-40-5) (250-25-50-5) (80+80+80+5+5) (50)

let draw_options (max_wager : int) last_call first =
  erase_box ();
  if last_call = 0 then begin
    draw_bet_raise false first;
    draw_call_check false first max_wager;
    draw_fold false; end
  else begin
    draw_fold false;
    draw_call_check false first max_wager end

let rec text_hover (first : bool) (max_wager : int) (last_call : int): string =
  let bet_input unit = 
    let rec text_input str : string =
      let draw_box str2 =
        set_color black;
        fill_rect (360-80-40-5) (250-25-50-30) (80+80+5+5+80) 20;
        set_color white;
        moveto (360-80-40) (250-25-50-25);
        draw_string ("> " ^ str2) in
      draw_box str;
      let stat2 = wait_next_event (Key_pressed :: []) in
      if stat2.key = '\027' then begin erase_box (); text_hover first max_wager last_call end
      else if stat2.key = '\r' then if String.length str = 0 then text_input "" else str 
      else if stat2.key = '\b' then if String.length str = 0 then text_input "" else text_input (String.sub str 0 (String.length str - 1))
      else if stat2.key = '0' || stat2.key = '1' || stat2.key = '2' || stat2.key = '3' || stat2.key = '4' || stat2.key = '5' || stat2.key = '6' || stat2.key = '7' || stat2.key = '8' || stat2.key = '9'
      then text_input (str ^ (Char.escaped stat2.key))
      else text_input str
    in
    text_input "" in
  let stat = wait_next_event (Button_down :: Mouse_motion :: []) in
  if stat.mouse_x > (360-80-40-5) && stat.mouse_x < (360-80-40-5+80) && stat.mouse_y > (250-25-50-5) && stat.mouse_y < (250-25-50-5+50)
  then begin draw_fold true; if stat.button then "Fold" else text_hover first max_wager last_call end
  else if stat.mouse_x > (360-40) && stat.mouse_x < (360-40+80) && stat.mouse_y > (250-25-50-5) && stat.mouse_y < (250-25-50-5+50)
  then begin draw_call_check true first max_wager; if stat.button then begin if first && last_call = 0 then "Check" else "Call" end else text_hover first max_wager last_call end
  else if last_call <> 1 && stat.mouse_x > (360-40+80+5) && stat.mouse_x < (360-40+80+5+80) && stat.mouse_y > (250-25-50-5) && stat.mouse_y < (250-25-50-5+50)
  then begin draw_bet_raise true first; if stat.button then begin if first then "Bet " ^ bet_input () else "Raise " ^ bet_input () end else text_hover first max_wager last_call end
  else begin draw_options max_wager last_call first; text_hover first max_wager last_call end

let rec request_choice max_wager (gametable : Table.table) round (p : Table.person) : unit =
  set_color yellow;
  draw_rect ((fst p.location)-40) ((snd p.location)-25) 80 50;
  if List.length gametable.in_players = 1 then () else 
    match p.position with
    | Some Folded | Some AllIn _ -> ()
    | _ ->
      if (p.name = "Bot 1" || p.name = "Bot 2" || p.name = "Bot 3" || p.name = "Bot 4" || p.name = "Bot 5")
      then let bot_bet = if gametable.last_call = 0 
             then bot_choice p max_wager 
             else bot_choice_fold p max_wager in
        match bot_bet with 
        | Fold -> p.position <- Some Folded; 
          print_choice bot_bet p; 
          draw_player p; 
          draw_pot gametable; 
          moveto ((fst p.location)-35) ((snd p.location)-20); 
          draw_string (string_of_choice bot_bet p); 
          Table.next_br_prep gametable 
        | AllIn x -> p.position <- Some (AllIn round); 
          bot_bet_opt max_wager gametable p bot_bet
        | _ -> bot_bet_opt max_wager gametable p bot_bet 

      else 
        match p.position with 
        | Some Folded   -> ()
        | Some (AllIn _) -> ()
        | _ ->
          begin
            try
              begin
                let input = if gametable.last_call = 1 then 
                    begin draw_options !max_wager 1 false; text_hover false !max_wager 1 end
                  else if !max_wager = 0 
                  then begin draw_options !max_wager 0 true; text_hover true !max_wager 0 end
                  else begin draw_options !max_wager 0 false; text_hover false !max_wager 0 end in
                erase_options ();
                let player_bet = parse input p max_wager in  
                let bet_check = Bet.check_wager player_bet !max_wager in
                set_color white;
                fill_rect (360-105) 100 210 13;
                match player_bet with
                | Fold -> p.position <- Some Folded; 
                  print_choice player_bet p; 
                  draw_player p; 
                  draw_pot gametable; 
                  moveto ((fst p.location)-35) ((snd p.location)-20); 
                  draw_string (string_of_choice player_bet p); 
                  Table.next_br_prep gametable 
                | AllIn x -> p.position <- Some (AllIn round); 
                  player_bet_opt max_wager gametable p player_bet bet_check
                | _ -> player_bet_opt max_wager gametable p player_bet bet_check

              end
            with 
            | Bet.InvalidResponse -> print_endline "Invalid Choice. Try again.";
              request_choice max_wager gametable round p 
            | Bet.InvalidWager -> print_endline "Invalid Wager Amount. Try again.";
              moveto (360-105) (100);
              set_color red;
              draw_string "Invalid Wager Amount. Select again.";
              request_choice max_wager gametable round p 
          end



