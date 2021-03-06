(** [card] represents a playing card with a rank from 1-13 and a suit
    denoted by the first character of the suit's name. A card with the rank of
    1 represents an ace, 11 is a Jack, 12 a Queen, and 13 a King.*)
type card = {rank : int; suit : char}

(** [deck] represents a mutable array of cards. *)
type deck = card array ref

(* [print_card card] is the string representation of the card [c]. *)
let print_card (card : card) : string = 
  let print_suit (c : char) : string =
    match c with
    | 'C' -> "Clubs"
    | 'H' -> "Hearts"
    | 'D' -> "Diamonds"
    | 'S' -> "Spades" 
    | _ -> failwith "Invalid suit" in
  match card with
  | {rank = 13; suit = c} -> "King of " ^ print_suit c
  | {rank = 12; suit = c} -> "Queen of " ^ print_suit c
  | {rank = 11; suit = c} -> "Jack of " ^ print_suit c
  | {rank = 1; suit = c} -> "Ace of " ^ print_suit c
  | {rank = n; suit = c} -> string_of_int n ^ " of " ^ print_suit c

(* [empty] is the empty deck. *)
let empty () = ref [||]

let push_unit (card : card) (deck : deck) = deck := Array.append [|card|] !deck

let push (card : card) (deck : deck) = 
  push_unit card deck; 
  deck

(* [peek deck] is the first element of [deck].*)
let peek (deck : deck) : 'a = !deck.(0)

(* [pop deck] removes and then returns the first element of [deck]. *)
let pop (deck : deck) : 'a = 
  let first = !deck.(0) in
  deck := Array.sub !deck 1 (Array.length !deck - 1);
  first

(* [shuffle d] randomizes the deck [d] using the Knuth shuffle a
   lgorithm before returning the shuffled deck. *)
let shuffle (deck : deck) = 
  Random.self_init ();
  let swap index1 index2 =
    let temp = Array.get !deck index1 in
    Array.set !deck index1 (Array.get !deck index2);
    Array.set !deck index2 temp in
  for i = Array.length !deck - 1 downto 1 do
    let j : int = Random.int (i + 1) in
    swap i j;
  done;
  deck

(* [create_help size] initializes a new deck s times with the standard 
   52-card deck.*)
let create_help size deck = 
  for i = 1 to 13 do
    push_unit {rank = i; suit = 'C'} deck;
    push_unit {rank = i; suit = 'D'} deck;
    push_unit {rank = i; suit = 'H'} deck;
    push_unit {rank = i; suit = 'S'} deck;
  done;
  shuffle deck

(* [create] calls [create_help 1] to initialize a new deck with the standard 
   52-card deck in random order. *)
let create () : deck =
  let deck = ref [||] in
  create_help 1 deck

let create_size size : deck =
  let deck = ref [||] in
  create_help size deck