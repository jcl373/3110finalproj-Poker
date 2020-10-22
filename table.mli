type pos =  
  | Dealer
  | BB
  | LB
  | Folded
  | Leave

(** The type [player] represents a player in the game. A player
    has a name, which is an identifier for the player, a hand, *)
type person = {name : string; mutable hand: Deck.card * Deck.card; 
               chips : int ref; mutable position : pos option } 




