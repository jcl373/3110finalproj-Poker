(** Handles player and bot input. *)

(** [request_choice max_wager gametable round player] requests the choice from 
    player [player] at table [gametable] in round [round] with maximum wager 
    [max_wager]. *)
val request_choice :  int ref -> Table.table ->  int -> Table.person  -> unit

(** [draw_player player] draws the status elements of player [player] on their 
    graphical pane. *)
val draw_player : Table.person -> unit

(** [draw_pot gametable] draws the current pot for table [gametable] in the 
    center of the table. *)
val draw_pot : Table.table -> unit