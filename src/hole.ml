open Deck
open Hand

type t = Card.t * Card.t

exception IllegalHole

let make_hole deck = (draw_card deck, draw_card deck)
let make_hole_cards [ c1; c2 ] = (c1, c2)

let get_first t =
  match t with
  | a, b -> a

let get_second t =
  match t with
  | a, b -> b

let get_cards (c1, c2) = [ c1; c2 ]

let is_pair t =
  let h1 = get_first t in
  let h2 = get_second t in
  Card.get_num h1 = Card.get_num h2
