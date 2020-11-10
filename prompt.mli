val parse : string -> Table.person -> int ref -> Bet.choice

val request_choice :  int ref -> Table.table ->  int -> Table.person  -> unit

val draw_player : Table.person -> unit

val draw_pot : Table.table -> unit