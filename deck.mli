(**IN PROGRESS *)


module type Deck = sig

  type t

  type d = t array ref

  val empty : d

  val push : t -> d -> unit

  val peek : d -> t

  val pop : d -> t



















end