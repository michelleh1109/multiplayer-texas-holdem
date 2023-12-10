type t = {
  mutable cards : Card.t list;
  mutable size : int;
}

exception IllegalCard

let rec create_deck num =
  if num <= 13 then
    [
      Card.make_card num 'S';
      Card.make_card num 'H';
      Card.make_card num 'D';
      Card.make_card num 'C';
    ]
    @ create_deck (num + 1)
  else []

let all_cards = ref (create_deck 1)
let empty_deck () = { cards = []; size = 0 }
let init_deck () = { cards = !all_cards; size = 52 }
let get_cards deck = deck.cards
let get_size deck = List.length deck.cards

let remove_card deck card =
  deck.cards <- List.filter (fun x -> card <> x) deck.cards;
  deck.size <- deck.size - 1;
  deck

let draw_card deck =
  let c = List.nth deck.cards (Random.int deck.size) in
  let _ = remove_card deck c in
  c
