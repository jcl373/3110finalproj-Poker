open Graphics

exception InvalidResponse

let bot_choice (player : Table.person) max_wager : Bet.choice =
  Random.self_init ();
  match Random.int 3 with 
  | 0 -> if !max_wager = 0 && !(player.chips) > 5 then Bet 5
    else if !max_wager < !(player.chips) then Call !max_wager
    else Fold
  | 1 -> if !max_wager = 0 && !(player.chips) > 5 then Bet 5
    else if !max_wager + 5 < !(player.chips) then Raise (!max_wager + 5) 
    else Fold
  | 2 -> if !max_wager = 0 && !(player.chips) > 5 then Check
    else if !(player.chips) > !max_wager then Call !max_wager
    else Fold
  | _ -> failwith "Impossible scenario"

let bot_choice_fold (player : Table.person) max_wager : Bet.choice =
  if !max_wager < !(player.chips) then Call !max_wager 
  else Fold

let string_of_choice (choice : Bet.choice) (player : Table.person) : string =
  match choice with
  | Check -> set_color yellow; "Check"
  | Fold -> set_color red; "Fold"
  | Bet i -> set_color green; "Bet " ^ string_of_int i
  | Call i -> set_color green; "Call " ^ string_of_int i
  | Raise i -> set_color green; "Raise " ^ string_of_int i
  | AllIn i -> set_color green; "All In"

let print_choice_helper i =
  string_of_int i ^ " chips. "

let print_choice (choice : Bet.choice) (player : Table.person) : unit =
  match choice with
  | Check -> print_endline (player.name ^ " has checked.")
  | Fold -> print_endline (player.name ^ " has folded.")
  | Bet i -> print_endline (player.name ^ " has bet " ^ 
                            print_choice_helper i)
  | Call i -> print_endline (player.name ^ " has called and bet " ^ 
                             print_choice_helper i)
  | Raise i -> print_endline (player.name ^ " has raised " ^ 
                              print_choice_helper i)
  | AllIn i -> print_endline (player.name ^ " has gone all in and bet " ^ 
                              print_choice_helper i)

let parse str (player : Table.person) max_wager : Bet.choice =
  let lst = String.split_on_char ' ' str in
  let lst = List.filter ((<>) "") lst in
  match lst with
  | [] -> raise(Bet.InvalidResponse) 
  | h :: t -> 
    begin 
      if !max_wager = 0 then 
        match h with 
        | "Check" -> Check
        | "Fold" -> Fold
        | "Bet" -> Bet (int_of_string (String.concat "" t))
        | "AllIn" | "Allin" -> AllIn !(player.chips)
        | _ -> raise(Bet.InvalidResponse) 
      else 
        match h with 
        | "Call" -> Call !max_wager
        | "Fold" -> Fold
        | "Raise" -> Raise (int_of_string (String.concat "" t))
        | "AllIn" | "Allin" -> AllIn !(player.chips)
        | _ -> raise(Bet.InvalidResponse) 
    end 

let draw_player (player : Table.person) =
  let x = fst player.location in
  let y = snd player.location in
  set_color (rgb 70 70 70);
  fill_rect (x-40) (y-25) 80 50;
  moveto (x-35) (y+10);
  set_color white;
  draw_string player.name;
  moveto (x-35) (y-5);
  draw_string (string_of_int !(player.chips));
  match player.position with
  | Some Folded -> moveto ((fst player.location)-35)
                     ((snd player.location)-20); 
    set_color red;
    draw_string "Fold";
    ()
  | Some AllIn x -> moveto ((fst player.location)-35) 
                      ((snd player.location)-20); 
    set_color green;
    draw_string ("All in " ^ string_of_int x);
    ()
  | _ -> ()

let draw_pot (gametable : Table.table) : unit =
  set_color (rgb 68 125 35);
  fill_rect 300 425 100 15;
  set_color white;
  moveto 300 425;
  draw_string ("Total pot: " ^ (string_of_int !(gametable.pot)))

let curr_bet max_wager (gametable : Table.table) (player : Table.person) =
  if gametable.last_call = 0 then !max_wager
  else !max_wager - player.last_bet

let player_bet_opt max_wager (gametable : Table.table) (player : Table.person) 
    (player_bet : Bet.choice) bet_check =
  if gametable.last_call = 0 then
    match player_bet with 
    | Raise x | Bet x | AllIn x -> gametable.last_bet <- Some player
    | _ -> ()
  else ();
  let curr_wager = Bet.current_wager player_bet in
  if bet_check = false || Bet.max_wager player_bet !(player.chips) = false
  then raise(Bet.InvalidWager) 
  else if (curr_wager > !max_wager) && (curr_wager <= !(player.chips))
  then max_wager := curr_wager;
  if gametable.last_call = 0 then player.last_bet <- curr_wager;
  Bet.wager player_bet gametable.pot player.chips 
    (curr_bet max_wager gametable player) !max_wager;
  draw_player player;
  draw_pot gametable; 
  moveto ((fst player.location)-35) ((snd player.location)-20); 
  draw_string (string_of_choice player_bet player);
  print_choice player_bet player

let bot_bet_opt max_wager (gametable : Table.table) (player : Table.person) 
    (bot_bet : Bet.choice) =
  Unix.sleepf 1.;
  if gametable.last_call = 0 then
    match bot_bet with 
    | Raise x | Bet x | AllIn x -> gametable.last_bet <- Some player
    | _ -> ()
  else ();
  let curr_wager = Bet.current_wager bot_bet in 
  if curr_wager > !max_wager then max_wager := curr_wager;
  if gametable.last_call = 0 then player.last_bet <- curr_wager;
  Bet.wager bot_bet gametable.pot player.chips 
    (curr_bet max_wager gametable player) !max_wager;
  draw_player player;
  draw_pot gametable; 
  moveto ((fst player.location)-35) ((snd player.location)-20); 
  draw_string (string_of_choice bot_bet player);
  print_choice bot_bet player

let draw_bet_raise (hover : bool) (bet : bool) =
  if hover then set_color (rgb 67 131 14) else set_color (rgb 87 175 13);
  fill_rect 405 170 80 50;
  set_color white;
  moveto 410 205;
  if bet then draw_string "Bet" else draw_string "Raise"

let draw_call_check (hover : bool) (check : bool) (max_wager : int) =
  if hover then set_color (rgb 188 153 0) else set_color (rgb 255 208 0);
  fill_rect 320 170 80 50;
  set_color white;
  moveto 325 205;
  if check then draw_string "Check" else begin
    draw_string "Call";
    moveto 325 190;
    draw_string (string_of_int max_wager) end

let draw_fold (hover : bool) =
  if hover then set_color (rgb 180 0 0) else set_color (rgb 220 40 0);
  fill_rect 235 170 80 50;
  set_color white;
  moveto 240 205;
  draw_string "Fold"

let erase_box unit =
  set_color white;
  fill_rect 235 145 250 20

let erase_options unit =
  erase_box ();
  fill_rect 235 170 250 50

let draw_options (max_wager : int) last_call first =
  erase_box ();
  if last_call = 0 then begin
    draw_bet_raise false first;
    draw_call_check false first max_wager;
    draw_fold false end
  else begin
    draw_fold false;
    draw_call_check false first max_wager end

let rec unclick unit =
  if button_down () then unclick () else ()

let rec text_hover (first : bool) (max_wager : int) (last_call : int) : string =
  let bet_input unit = 
    let rec text_input str : string =
      let draw_box str2 =
        set_color black;
        fill_rect 235 145 250 20;
        set_color white;
        moveto 240 150;
        draw_string ("> " ^ str2) in
      draw_box str;
      let stat2 = wait_next_event (Key_pressed :: []) in
      if stat2.key = '\027' then begin 
        erase_box (); 
        text_hover first max_wager last_call end
      else if stat2.key = '\r' then 
        if String.length str = 0 then text_input "" else str 
      else if stat2.key = '\b' then 
        if String.length str = 0 then text_input "" 
        else text_input (String.sub str 0 (String.length str - 1))
      else if stat2.key = '0' || stat2.key = '1' || stat2.key = '2' || 
              stat2.key = '3' || stat2.key = '4' || stat2.key = '5' || 
              stat2.key = '6' || stat2.key = '7' || stat2.key = '8' || 
              stat2.key = '9' then text_input (str ^ (Char.escaped stat2.key))
      else text_input str in
    text_input "" in
  auto_synchronize false;
  let stat = wait_next_event (Button_down :: Mouse_motion :: Poll :: []) in
  let call_check unit = if first && last_call = 0 then "Check" else "Call" in
  let bet_raise unit = if first then "Bet " ^ bet_input () 
    else "Raise " ^ bet_input () in
  if stat.mouse_x > 235 && stat.mouse_x < 315 && 
     stat.mouse_y > 170 && stat.mouse_y < 220
  then begin 
    draw_fold true; 
    auto_synchronize true; 
    if stat.button then "Fold" else text_hover first max_wager last_call end
  else if stat.mouse_x > 320 && stat.mouse_x < 400 && 
          stat.mouse_y > 170 && stat.mouse_y < 220
  then begin 
    draw_call_check true first max_wager; 
    auto_synchronize true; 
    if stat.button then call_check () 
    else text_hover first max_wager last_call end
  else if last_call <> 1 && stat.mouse_x > 405 && stat.mouse_x < 485 && 
          stat.mouse_y > 170 && stat.mouse_y < 220
  then begin 
    draw_bet_raise true first; 
    auto_synchronize true; 
    if stat.button then bet_raise () 
    else text_hover first max_wager last_call end
  else begin 
    draw_options max_wager last_call first; 
    auto_synchronize true; 
    text_hover first max_wager last_call end

let rc_setup (gametable : Table.table) player max_wager bot = 
  if bot then
    if gametable.last_call = 0 
    then bot_choice player max_wager 
    else bot_choice_fold player max_wager 
  else begin 
    let input = if gametable.last_call = 1 then begin
        draw_options !max_wager 1 false; 
        unclick (); 
        text_hover false !max_wager 1 end
      else if !max_wager = 0 
      then begin
        draw_options !max_wager 0 true; 
        unclick (); 
        text_hover true !max_wager 0 end
      else begin
        draw_options !max_wager 0 false; 
        unclick (); 
        text_hover false !max_wager 0 end in
    erase_options (); 
    parse input player max_wager end

let request_choice_help round (gametable : Table.table) player max_wager bot =
  let player_bet = rc_setup gametable player max_wager bot in 
  let bet_check = Bet.check_wager player_bet !max_wager in
  set_color white; fill_rect 255 100 210 13;
  match player_bet with
  | Fold -> player.position <- Some Folded; 
    print_choice player_bet player; 
    draw_player player; 
    draw_pot gametable; 
    moveto ((fst player.location)-35) ((snd player.location)-20); 
    draw_string (string_of_choice player_bet player); 
    Table.next_br_prep gametable 
  | AllIn x -> player.position <- Some (AllIn round); 
    if bot then bot_bet_opt max_wager gametable player player_bet 
    else player_bet_opt max_wager gametable player player_bet bet_check
  | _ -> if bot then bot_bet_opt max_wager gametable player player_bet 
    else player_bet_opt max_wager gametable player player_bet bet_check

let is_bot (player : Table.person) = 
  player.name = "Bot 1" || 
  player.name = "Bot 2" || 
  player.name = "Bot 3" || 
  player.name = "Bot 4" || 
  player.name = "Bot 5"

let rec request_choice max_wager (gametable : Table.table) round 
    (player : Table.person) : unit =
  let resolve unit = begin
    try request_choice_help round gametable player max_wager (is_bot player) 
    with 
    | Bet.InvalidResponse -> print_endline "Invalid Choice. Try again.";
      request_choice max_wager gametable round player 
    | Bet.InvalidWager -> print_endline "Invalid Wager Amount. Try again.";
      moveto 255 100;
      set_color red;
      draw_string "Invalid Wager Amount. Select again.";
      request_choice max_wager gametable round player end in
  set_color yellow;
  draw_rect ((fst player.location)-40) ((snd player.location)-25) 80 50;
  if List.length gametable.in_players = 1 then () else 
    match player.position with
    | Some Folded | Some AllIn _ -> ()
    | _ -> resolve ()