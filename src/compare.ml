open Hand
open Card

exception IllegalHand
exception InexhaustivePatterns

(* [card_compare] takes in two cards and returns 1 if first card is larger, 0 if
   tied, and -1 o/w*)
let card_compare c1 c2 = compare (Card.get_num c1) (Card.get_num c2)

(* [card sort] helper function sorts a list of cards from smallest to largest
   using their number *)
let card_sort lst = List.sort card_compare lst

(* [compare_straight_flush] returns winner for a straight, flush, or straight
   flush *)
let rec compare_straight_flush p1 p2 h1 h2 =
  match h1 with
  | [] -> p1
  | h1 :: t1 -> (
      let c1 = Card.get_num h1 in
      match h2 with
      | [] -> p1
      | h2 :: t2 ->
          let c2 = Card.get_num h2 in
          if c1 = c2 then compare_straight_flush p1 p2 t1 t2
          else if c1 > c2 then p1
          else p2)

(* [compare_four_of_a_kind] compare tied four of a kind *)
let compare_four_of_a_kind p1 p2 h1 h2 =
  match (h1, h2) with
  | n1 :: _, n2 :: _ when n1 > n2 -> p1
  | n1 :: _, n2 :: _ when n2 > n1 -> p2
  | _ :: _ :: _ :: _ :: p1n5, _ :: _ :: _ :: _ :: p2n5 when p1n5 > p2n5 -> p1
  | _ :: _ :: _ :: _ :: p1n5, _ :: _ :: _ :: _ :: p2n5 when p1n5 < p2n5 -> p2
  | _ -> p1

(* [compare_triples] returns winner with full house or triples *)
let compare_triples p1 p2 h1 h2 =
  match (h1, h2) with
  | n1 :: _, n2 :: _ when n1 > n2 -> p1
  | n1 :: _, n2 :: _ when n2 > n1 -> p2
  | _ :: _ :: _ :: p1n4 :: _, _ :: _ :: _ :: p2n4 :: _ when p1n4 > p2n4 -> p1
  | _ :: _ :: _ :: p1n4 :: _, _ :: _ :: _ :: p2n4 :: _ when p1n4 < p2n4 -> p2
  | _ :: _ :: _ :: _ :: p1n5, _ :: _ :: _ :: _ :: p2n5 when p1n5 > p2n5 -> p1
  | _ :: _ :: _ :: _ :: p1n5, _ :: _ :: _ :: _ :: p2n5 when p1n5 < p2n5 -> p2
  | _ -> p1

(* [compare_two_pair] compares hand with two pairs and the kicker card *)
let rec compare_two_pair p1 p2 h1 h2 =
  match (h1, h2) with
  | n1 :: _, n2 :: _ when n1 > n2 -> p1
  | n1 :: _, n2 :: _ when n2 > n1 -> p2
  | _ :: _ :: p1n3 :: _ :: _, _ :: _ :: p2n3 :: _ :: _ when p1n3 > p2n3 -> p1
  | _ :: _ :: p1n3 :: _ :: _, _ :: _ :: p2n3 :: _ :: _ when p1n3 < p2n3 -> p2
  | _ :: _ :: _ :: _ :: p1n5, _ :: _ :: _ :: _ :: p2n5 when p1n5 > p2n5 -> p1
  | _ :: _ :: _ :: _ :: p1n5, _ :: _ :: _ :: _ :: p2n5 when p1n5 < p2n5 -> p2
  | _ -> p1

(* [compare_singles] returns winning player of all single cards *)
let rec compare_singles p1 p2 lst1 lst2 =
  match lst1 with
  | [] -> p1
  (* should be a tie *)
  | h1 :: t1 -> begin
      match lst2 with
      | [] -> p1
      (* should be a tie *)
      | h2 :: t2 ->
          if h1 > h2 then p1
          else if h2 > h1 then p2
          else compare_singles p1 p2 t1 t2
    end

(* [compare_pair] compares hands with pairs *)
let rec compare_pair p1 p2 h1 h2 =
  match (h1, h2) with
  | n1 :: _, n2 :: _ when n1 > n2 -> p1
  | n1 :: _, n2 :: _ when n2 > n1 -> p2
  | _ :: _ :: p1n3 :: _ :: _, _ :: _ :: p2n3 :: _ :: _ when p1n3 > p2n3 -> p1
  | _ :: _ :: p1n3 :: _ :: _, _ :: _ :: p2n3 :: _ :: _ when p1n3 < p2n3 -> p2
  | _ :: _ :: _ :: p1n4 :: _, _ :: _ :: _ :: p2n4 :: _ when p1n4 > p2n4 -> p1
  | _ :: _ :: _ :: p1n4 :: _, _ :: _ :: _ :: p2n4 :: _ when p1n4 < p2n4 -> p2
  | _ :: _ :: _ :: _ :: p1n5, _ :: _ :: _ :: _ :: p2n5 when p1n5 > p2n5 -> p1
  | _ :: _ :: _ :: _ :: p1n5, _ :: _ :: _ :: _ :: p2n5 when p1n5 < p2n5 -> p2
  | _ :: _ :: _ :: _ :: p1n5, _ :: _ :: _ :: _ :: p2n5 when p1n5 > p2n5 -> p1
  | _ :: _ :: _ :: _ :: p1n5, _ :: _ :: _ :: _ :: p2n5 when p1n5 < p2n5 -> p2
  | _ -> p1

(* [compare_hand] compares tied hands to return winner *)
let compare_hand p1 p2 rank h1 h2 =
  match rank with
  | 10 -> p1
  | 9 ->
      compare_straight_flush p1 p2
        (List.rev (card_sort h1))
        (List.rev (card_sort h2))
  | 8 -> compare_four_of_a_kind p1 p2 h1 h2
  | 7 -> compare_triples p1 p2 h1 h2
  | 6 ->
      compare_straight_flush p1 p2
        (List.rev (card_sort h1))
        (List.rev (card_sort h2))
  | 5 ->
      compare_straight_flush p1 p2
        (List.rev (card_sort h1))
        (List.rev (card_sort h2))
  | 4 -> compare_triples p1 p2 h1 h2
  | 3 -> compare_two_pair p1 p2 h1 h2
  | 2 -> compare_pair p1 p2 h1 h2
  | 1 ->
      compare_singles p1 p2 (List.rev (card_sort h1)) (List.rev (card_sort h2))
  | _ -> raise InexhaustivePatterns

(* [find_winner] returns winning player*)
let find_winner p1 p2 hand =
  let r1, h1 =
    Hand.get_rank (make_hand (Hole.get_cards (Player.get_hole p1) @ hand))
  in
  let r2, h2 =
    Hand.get_rank (make_hand (Hole.get_cards (Player.get_hole p2) @ hand))
  in
  if r1 > r2 then p1 else if r2 < r1 then p2 else compare_hand p1 p2 r1 h1 h2
