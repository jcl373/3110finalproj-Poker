let gametable = Table.empty_table 5 10
let gamedeck = Deck.create

let start_game name = 
  let rec round = 
    Table.next_round_prep gametable;
    print_endline ("The current dealer is " ^ (Table.extract_value gametable.dealer).name) in
  Table.add_player gametable (Table.new_player name (Deck.pop gamedeck) (Deck.pop gamedeck) 100);
  Table.add_player gametable (Table.new_player "Bot 1" (Deck.pop gamedeck) (Deck.pop gamedeck) 100);
  Table.add_player gametable (Table.new_player "Bot 2" (Deck.pop gamedeck) (Deck.pop gamedeck) 100);
  Table.add_player gametable (Table.new_player "Bot 3" (Deck.pop gamedeck) (Deck.pop gamedeck) 100);
  Table.add_player gametable (Table.new_player "Bot 4" (Deck.pop gamedeck) (Deck.pop gamedeck) 100);
  Table.add_player gametable (Table.new_player "Bot 5" (Deck.pop gamedeck) (Deck.pop gamedeck) 100);
  Table.choose_dealer gametable;
  round

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