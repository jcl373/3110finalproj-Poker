module Deck = struct
  type card = {rank : int; suit : char}

  type 'a deck = 'a array ref

  let empty = ref [||]

  let push (c : 'a) (d : 'a deck) : unit = d := (Array.append [|c|] !d)

  let peek (d : 'a deck) : 'a = !d.(0)

  let pop (d : 'a deck) : 'a = let first = !d.(0) in
    d := Array.sub !d 1 (Array.length !d - 1);
    first

  let shuffle d = 
    let swap index1 index2 =
      let temp = Array.get !d index1 in
      Array.set !d index1 (Array.get !d index2);
      Array.set !d index2 temp in
    for i = Array.length !d - 1 downto 1 do
      let j : int = Random.int (i + 1) in
      swap i j;
    done
end