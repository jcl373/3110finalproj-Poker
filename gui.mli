(** Handles drawing graphical elements, *)

(** [max_name_len] is the maximum allowed length of a name string. *)
val max_name_len : int

(** [name_input ()] requests and then outputs the players name as a string. *)
val name_input : 'a -> string

(** [six_locations] are the coordinates for the graphical panes for the 
    six players. *)
val six_locations : (int * int) array

(** [draw_table ()] draws the poker table in the window background. *)
val draw_table : unit -> unit

(** [draw_card c x y] draws the card [c] at the coordinates [x],[y]. *)
val draw_card : Deck.card -> int -> int -> unit

(** [draw_table_cards t] draws the cards in the river for table [t]. *)
val draw_table_cards : Table.table -> unit

(** [draw_player_cards p] draws the cards in the hand of player [p]. *)
val draw_player_cards : Table.person -> unit

(** [draw_dealer p] draws the dealer tag above player [p]. *)
val draw_dealer : Table.person -> unit

(** [draw_winner p] draws the winner tag above player [p]. *)
val draw_winner : Table.person -> unit

(** [draw_players players i] recursively draws the graphical panes for the 
    players in list [players]. *)
val draw_players : Table.person list -> int -> unit

(** [draw_blinds b gametable f] draws the blind bet for player [b] in table 
    [gametable] based on the bet values from [f] *)
val draw_blinds : Table.person -> Table.table -> (int * int -> int) -> unit