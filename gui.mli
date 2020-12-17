val max_name_len : int

val draw_box : string -> unit

val name_input : 'a -> string

val six_locations : (int * int) array

val draw_table : unit -> unit

val draw_card : Deck.card -> int -> int -> unit

val draw_table_cards : Table.table -> unit

val draw_player_cards : Table.person -> unit

val draw_dealer : Table.person -> unit

val draw_winner : Table.person -> unit

val draw_players : Table.person list -> int -> unit

val draw_blinds : Table.person -> Table.table -> (int * int -> int) -> unit