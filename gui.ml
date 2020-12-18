include Graphics

let max_name_len = 12

let draw_box str =
  set_color black;
  fill_rect 200 350 320 20;
  set_color white;
  moveto 205 355;
  draw_string ("> " ^ str);
  if String.length str = max_name_len then begin 
    moveto 455 355; 
    set_color red; 
    draw_string "Max length" end

let name_input unit = 
  let rec text_input str : string =
    draw_box str;
    let stat = wait_next_event (Key_pressed :: []) in
    if stat.key = '\027' || stat.key = '\r' then str 
    else if stat.key = '\b' then 
      String.length str - 1 
      |> String.sub str 0 
      |> text_input 
    else if String.length str >= max_name_len then text_input str
    else text_input (str ^ (Char.escaped stat.key)) in
  text_input ""

let six_locations = 
  [|(360,250);(175,275);(175,445);(360,470);(545,445);(545,275)|]

let draw_table () =
  set_color (rgb 68 125 35);
  fill_rect 235 235 250 250;
  fill_circle 235 360 125;
  fill_circle 485 360 125

let draw_card (card : Deck.card) (x : int) (y : int) =
  set_color white;
  fill_rect x y 38 60;
  if card.suit = 'C' || card.suit = 'S' then set_color black else set_color red;
  moveto (x + 5) (y + 45);
  (match card.rank with
   | 1 -> draw_string "A"
   | 11 -> draw_string "J"
   | 12 -> draw_string "Q"
   | 13 -> draw_string "K"
   | n -> draw_string (string_of_int n));
  moveto (x + 5) (y + 15);
  draw_string (Char.escaped card.suit)

let draw_table_cards (table : Table.table) =
  let rec cards (card : Deck.card list) (i : int) =
    match card with
    | [] -> ()
    | h :: t -> begin 
        draw_card h (245 + i * 48) 330;
        cards t (i + 1) end in
  cards table.river 0

let draw_player_cards (player : Table.person) =
  set_color white;
  draw_card (fst player.hand) 409 240;
  draw_card (snd player.hand) 457 240

let draw_dealer (player : Table.person) =
  Prompt.draw_player player;
  set_color (rgb 200 200 200);
  fill_rect (fst (player.location) - 40) (snd (player.location) + 25) 80 15;
  moveto (fst (player.location) - 35) (snd (player.location) + 25);
  set_color black;
  draw_string "Dealer"

let draw_winner (player : Table.person) =
  Prompt.draw_player player;
  set_color yellow;
  fill_rect (fst (player.location) - 40) (snd (player.location) + 25) 80 15;
  moveto (fst (player.location) - 35) (snd (player.location) + 25);
  set_color black;
  draw_string "Winner"

let rec draw_players (players : Table.person list) (i : int) =
  match players with
  | [] -> ()
  | h :: t -> Prompt.draw_player h; draw_players t (i+1)

let draw_blinds blind (gametable : Table.table) f =
  Prompt.draw_player blind; set_color yellow; 
  moveto ((fst blind.location) - 35) ((snd blind.location) - 20); 
  draw_string ("Blind bet " ^ string_of_int (f gametable.blinds));
  Prompt.draw_pot gametable;
  Unix.sleepf 0.5