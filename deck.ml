module Deck = struct
  (** The type [card] represents a playing card with a rank from 1-13 and a suit
      denoted by the first character of the suit's name *)
  type card = {rank : int; suit : char}

  (** The type [deck] represents a mutable array of cards. *)
  type deck = card array ref

  (* [empty] is the empty deck *)
  let empty = ref [||]

  (* To push [c] onto [d], we create a new array with value [c] at the 
     beginning followed by the values of [d]. *)
  let push (c : card) (d : deck) : unit = d := (Array.append [|c|] !d)

  (* [peek d] is the first element of [d].*)
  let peek (d : deck) : 'a = !d.(0)

  (* [pop d] removes and then returns the first element of [d]. *)
  let pop (d : deck) : 'a = let first = !d.(0) in
    d := Array.sub !d 1 (Array.length !d - 1);
    first

  (* [shuffle d] randomizes the deck [d] using the Knuth shuffle algorithm. *)
  let shuffle (d : deck) = 
    let swap index1 index2 =
      let temp = Array.get !d index1 in
      Array.set !d index1 (Array.get !d index2);
      Array.set !d index2 temp in
    for i = Array.length !d - 1 downto 1 do
      let j : int = Random.int (i + 1) in
      swap i j;
    done

  (* [create] initializes a new deck with the standard 52-card deck in 
     random order*)
  let create : deck =
    let d = empty in
    for i = 1 to 13 do
      push {rank = i; suit = 'C'} d;
      push {rank = i; suit = 'D'} d;
      push {rank = i; suit = 'H'} d;
      push {rank = i; suit = 'S'} d
    done;
    shuffle d;
    d;
end