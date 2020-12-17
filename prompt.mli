(** Handles player and bot input. *)

(** [request_choice max_wager gametable round p] requests the choice from 
    player [p] at table [gametable] in round [round] with maximum wager 
    [max_wager]. *)
val request_choice :  int ref -> Table.table ->  int -> Table.person  -> unit

(** [draw_player p] draws the status elements of player [p] on their 
    graphical pane. *)
val draw_player : Table.person -> unit

(** [draw_pot t] draws the current pot for table [t] in the center of the 
    table. *)
val draw_pot : Table.table -> unit