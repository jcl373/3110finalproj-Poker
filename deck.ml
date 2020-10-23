(** The type [card] represents a playing card with a rank from 1-13 and a suit
    denoted by the first character of the suit's name. A card with the rank of
    1 represents an ace, 11 is a Jack, 12 a Queen, and 13 a King.*)
type card = {rank : int; suit : char}

(** The type [deck] represents a mutable array of cards. *)
type deck = card array ref

(* [print_card c] is the string representation of the card [c] *)
let print_card (c : card) : string = 
  let print_suit (c : char) : string =
    match c with
    | 'C' -> "Clubs"
    | 'H' -> "Hearts"
    | 'D' -> "Diamonds"
    | 'S' -> "Spades" 
    | _ -> failwith "invalid suit" in
  match c with
  | {rank = 13; suit = c} -> "King of " ^ print_suit c
  | {rank = 12; suit = c} -> "Queen of " ^ print_suit c
  | {rank = 11; suit = c} -> "Jack of " ^ print_suit c
  | {rank = 1; suit = c} -> "Ace of " ^ print_suit c
  | {rank = n; suit = c} -> string_of_int n ^ " of " ^ print_suit c

(* [empty] is the empty deck *)
let empty () = ref [||]

(* To push [c] onto [d], we create a new array with value [c] at the 
   beginning followed by the values of [d]. *)
let push (c : card) (d : deck) = d := (Array.append [|c|] !d)

(* [peek d] is the first element of [d].*)
let peek (d : deck) : 'a = !d.(0)

(* [pop d] removes and then returns the first element of [d]. *)
let pop (d : deck) : 'a = let first = !d.(0) in
  d := Array.sub !d 1 (Array.length !d - 1);
  first

(* [shuffle d] randomizes the deck [d] using the Knuth shuffle a
   lgorithm before returning the shuffled deck. *)
let shuffle (d : deck) = 
  Random.init (int_of_float (Unix.gettimeofday ()));
  let swap index1 index2 =
    let temp = Array.get !d index1 in
    Array.set !d index1 (Array.get !d index2);
    Array.set !d index2 temp in
  for i = Array.length !d - 1 downto 1 do
    let j : int = Random.int (i + 1) in
    swap i j;
  done;
  d

(* [create_help s] initializes a new deck s times with the standard 
   52-card deck.*)
let create_help s = 
  let d = ref [||] in
  for i = 1 to 13 do
    push {rank = i; suit = 'C'} d;
    push {rank = i; suit = 'D'} d;
    push {rank = i; suit = 'H'} d;
    push {rank = i; suit = 'S'} d;
  done;
  shuffle d

(* [create] calls [create_help 1] to initialize a new deck with the standard 
   52-card deck in random order. *)
let create : deck =
  create_help 1 

(* [create_size s] calls [create_help s] to intialize a new deck with s number 
   of standard 52-card decks in random order.
   [s] is an int >= 1*)
let create_size s : deck =
  create_help s
