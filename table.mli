open Bet
(** The type [player] represents a player in the game. A player
has a name, which is an identifier for the player, a hand, *)
type person = {name : string; hand: Deck.card * Deck.card; money : Bet.bag} 



