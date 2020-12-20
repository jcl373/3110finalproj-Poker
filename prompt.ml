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

let parse_h1 (player : Table.person) hd tl : Bet.choice = 
  match hd with
  | "Check" -> Check
  | "Fold" -> Fold
  | "Bet" -> Bet (int_of_string (String.concat "" tl))
  | "Allin" -> AllIn !(player.chips)
  | _ -> raise(Bet.InvalidResponse)

let parse_h2 (player: Table.person) hd tl max_wager : Bet.choice = 
  match hd with
  | "Call" -> Call !max_wager
  | "Fold" -> Fold
  | "Raise" -> Raise (int_of_string (String.concat "" tl))
  | "Allin" -> AllIn !(player.chips)
  | _ -> raise(Bet.InvalidResponse) 

let parse str (player : Table.person) max_wager : Bet.choice =
  let lst = String.split_on_char ' ' str in
  let lst_filtered = List.filter ((<>) "") lst in
  match lst_filtered with
  | [] -> raise(Bet.InvalidResponse) 
  | h :: t -> if !max_wager = 0 then parse_h1 player h t 
    else parse_h2 player h t max_wager

let draw_player (player : Table.person) =
  let x = fst player.location in
  let y = snd player.location in
  set_color (rgb 70 70 70);
  fill_rect (x-40) (y-25) 80 50;

  set_color white;
  moveto (x-35) (y+10);
  draw_string player.name;
  moveto (x-35) (y-5);
  draw_string (string_of_int !(player.chips));

  moveto ((fst player.location)-35) ((snd player.location)-20); 
  match player.position with
  | Some Folded -> set_color red;
    draw_string "Fold"
  | Some AllIn x -> set_color green;
    draw_string ("All in " ^ string_of_int x)
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

let draw_raise (hover : bool) (bet : bool) =
  if hover then set_color (rgb 67 131 14) else set_color (rgb 87 175 13);
  fill_rect 405 170 80 50;

  set_color white;
  moveto 410 205;
  if bet then draw_string "Bet" else draw_string "Raise";

  auto_synchronize true

let draw_call (hover : bool) (check : bool) (max_wager : int) =
  if hover then set_color (rgb 188 153 0) else set_color (rgb 255 208 0);
  fill_rect 320 170 80 50;

  set_color white;
  moveto 325 205;
  if check then draw_string "Check" else begin
    draw_string "Call";
    moveto 325 190;
    draw_string (string_of_int max_wager) end;

  auto_synchronize true

let draw_fold (hover : bool) =
  if hover then set_color (rgb 180 0 0) else set_color (rgb 220 40 0);
  fill_rect 235 170 80 50;

  set_color white;
  moveto 240 205;
  draw_string "Fold";

  auto_synchronize true

let erase_box unit =
  set_color white;
  fill_rect 235 145 250 20

let erase_options unit =
  erase_box ();
  fill_rect 235 170 250 50

let draw_options (max_wager : int) last_call first =
  erase_box ();
  if last_call = 0 then begin
    draw_raise false first;
    draw_call false first max_wager;
    draw_fold false end
  else begin
    draw_fold false;
    draw_call false first max_wager end;
  auto_synchronize true

let rec unclick unit =
  if button_down () then unclick () else ()

let draw_hover_box str = 
  set_color black;
  fill_rect 235 145 250 20;

  set_color white;
  moveto 240 150;
  draw_string ("> " ^ str)

(* requires x1 <= x2 and y1 <= y2 *)
let in_box (stat : Graphics.status) ((x1, y1),(x2, y2)) =
  stat.mouse_x > x1 && stat.mouse_x < x2 && 
  stat.mouse_y > y1 && stat.mouse_y < y2

let rec text_input str : string =
  draw_hover_box str;
  let stat2 = wait_next_event (Key_pressed :: []) in
  if stat2.key = '\027' then begin
    erase_box (); 
    "RETRY" end
  else if stat2.key = '\r' then 
    if String.length str = 0 then text_input "" 
    else begin print_endline (String.escaped str); str end
  else if stat2.key = '\b' then 
    if String.length str = 0 then text_input "" 
    else text_input (String.sub str 0 (String.length str - 1))
  else if int_of_char stat2.key >= 48 && int_of_char stat2.key <= 57
  then text_input (str ^ (Char.escaped stat2.key))
  else text_input str

let rec text_hover (first : bool) (max_wager : int) (last_call : int) : string = 
  auto_synchronize false;
  let stat = wait_next_event (Button_down :: Mouse_motion :: Poll :: []) in
  let call unit = if first && last_call = 0 then "Check" else "Call" in
  let raise unit =
    let input = text_input "" in 
    if input = "RETRY" then text_hover first max_wager last_call 
    else begin if first then "Bet " ^ input else "Raise " ^ input end in
  if in_box stat ((235, 170), (315, 220)) then begin 
    draw_fold true; 
    if stat.button then "Fold" else text_hover first max_wager last_call end
  else if in_box stat ((320, 170), (400, 220)) then begin 
    draw_call true first max_wager; 
    if stat.button then call () else text_hover first max_wager last_call end
  else if last_call <> 1 && in_box stat ((405, 170), (485, 220)) then begin 
    draw_raise true first;
    if stat.button then raise () else text_hover first max_wager last_call end
  else begin 
    draw_options max_wager last_call first; 
    text_hover first max_wager last_call end

let request_choice_setup (gametable : Table.table) player max_wager bot = 
  if bot then
    if gametable.last_call = 0 then bot_choice player max_wager 
    else bot_choice_fold player max_wager 
  else begin 
    let input = if gametable.last_call = 1 then begin
        draw_options !max_wager 1 false; 
        unclick (); 
        text_hover false !max_wager 1 end
      else if !max_wager = 0 then begin
        draw_options !max_wager 0 true; 
        unclick (); 
        text_hover true !max_wager 0 end
      else begin
        draw_options !max_wager 0 false; 
        unclick (); 
        text_hover false !max_wager 0 end in
    erase_options ();
    print_endline input; 
    parse input player max_wager end

let request_choice_help round (gametable : Table.table) player max_wager bot =
  set_color white; 
  fill_rect 255 100 210 13;

  let player_bet = request_choice_setup gametable player max_wager bot in 
  let bet_check = Bet.check_wager player_bet !max_wager in
  let bet unit = if bot then bot_bet_opt max_wager gametable player player_bet 
    else player_bet_opt max_wager gametable player player_bet bet_check in
  match player_bet with
  | Fold -> player.position <- Some Folded; 
    print_choice player_bet player; 
    draw_player player; 
    draw_pot gametable; 
    moveto ((fst player.location)-35) ((snd player.location)-20); 
    draw_string (string_of_choice player_bet player); 
    Table.next_br_prep gametable 
  | AllIn x -> player.position <- Some (AllIn round); 
    bet ()
  | _ -> bet ()

let is_bot (player : Table.person) = 
  player.name = "Bot 1" || 
  player.name = "Bot 2" || 
  player.name = "Bot 3" || 
  player.name = "Bot 4" || 
  player.name = "Bot 5"

let rec request_choice max_wager (gametable : Table.table) round 
    (player : Table.person) : unit =
  set_color yellow;
  draw_rect ((fst player.location)-40) ((snd player.location)-25) 80 50;

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
  if List.length gametable.in_players = 1 then () else 
    match player.position with
    | Some Folded | Some AllIn _ -> ()
    | _ -> resolve ()