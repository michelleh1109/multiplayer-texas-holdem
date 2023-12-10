open Deck

type t = Card.t list

let draw_hand (n : int) (deck : Deck.t) =
  let rec countdown (i : int) (l : Card.t list) =
    if i < 1 then l else List.cons (draw_card deck) (countdown (i - 1) l)
  in
  countdown n []

let rec get_n (hand : t) (n : int) =
  if n = 0 then []
  else
    match hand with
    | [] -> failwith "empty hand"
    | h :: t -> get_n t (n - 1) @ [ h ]

let make_hand l = l
let hand_size h = List.length h
let get_hand (hand : t) = hand
let add_hands h1 h2 = h1 @ h2

(* ranking helpers *)

(** [sort_cards h] sorts the cards in h in order of descending. 1 is the highest
    number, 2 is the lowest*)
let rec sort_cards h =
  List.sort
    (fun c1 c2 ->
      let n1 = Card.get_num c1 in
      let n2 = Card.get_num c2 in
      match (n1, n2) with
      | 1, 1 -> 0
      | 1, _ -> -1
      | _, 1 -> 1
      | _ -> n2 - n1)
    h

(** [sort_cards_uniq h] sorts the cards in h in order of descending. 1 is the
    highest number, 2 is the lowest. Removes duplicates*)
let rec sort_cards_uniq h =
  List.sort_uniq
    (fun c1 c2 ->
      let n1 = Card.get_num c1 in
      let n2 = Card.get_num c2 in
      match (n1, n2) with
      | 1, 1 -> 0
      | 1, _ -> -1
      | _, 1 -> 1
      | _ -> n2 - n1)
    h

(** [is_n_kind n h] returns true if [h] contains [n] cards with the same number*)
let rec is_n_kind n h =
  let sorted_h = sort_cards h in
  if List.length sorted_h < n then false
  else if
    List.for_all
      (fun c1 -> Card.get_num c1 = Card.get_num (List.hd sorted_h))
      (get_n sorted_h n)
  then true
  else is_n_kind n (List.tl sorted_h)

(** [num_list c] returns the list of numbers in [c]*)
let rec num_list (c : Card.t list) =
  match c with
  | [] -> []
  | h :: t -> Card.get_num h :: num_list t

(** [suit_list c] returns the list of suits in [c]*)
let rec suit_list (c : Card.t list) =
  match c with
  | [] -> []
  | h :: t -> Card.get_suit h :: suit_list t

(** [diff l1 l2] is the difference between [l1] and [l2]*)
let rec diff l1 l2 =
  match l1 with
  | [] -> []
  | h :: t ->
      if not (List.mem h l2) then h :: diff t l2
      else diff t (List.filter (fun x -> x <> h) l2)

(** [remove_num n h] removes all the cards with number [n] from h*)
let rec remove_num n h =
  match h with
  | [] -> []
  | h :: t -> if Card.get_num h = n then remove_num n t else h :: remove_num n t

let pair h = is_n_kind 2 h

(** [find_n_kind] is the [n] cards that all have the same number*)
let rec find_n_kind n h =
  let sorted_h = sort_cards h in
  if List.length sorted_h < n then failwith "Not enough cards"
  else if
    List.for_all
      (fun c1 -> Card.get_num c1 = Card.get_num (List.hd sorted_h))
      (get_n sorted_h n)
  then get_n sorted_h n
  else find_n_kind n (List.tl sorted_h)

let two_pair h =
  if pair h then
    let p1 = find_n_kind 2 h in
    let no_p1 = remove_num (Card.get_num (List.hd p1)) h in
    pair no_p1
  else false

let three_of_a_kind h = is_n_kind 3 h

let straight h =
  List.length (diff [ 1; 2; 3; 4; 5 ] (num_list h)) = 0
  || List.length (diff [ 2; 3; 4; 5; 6 ] (num_list h)) = 0
  || List.length (diff [ 3; 4; 5; 6; 7 ] (num_list h)) = 0
  || List.length (diff [ 4; 5; 6; 7; 8 ] (num_list h)) = 0
  || List.length (diff [ 5; 6; 7; 8; 9 ] (num_list h)) = 0
  || List.length (diff [ 6; 7; 8; 9; 10 ] (num_list h)) = 0
  || List.length (diff [ 7; 8; 9; 10; 11 ] (num_list h)) = 0
  || List.length (diff [ 8; 9; 10; 11; 12 ] (num_list h)) = 0
  || List.length (diff [ 9; 10; 11; 12; 13 ] (num_list h)) = 0
  || List.length (diff [ 10; 11; 12; 13; 1 ] (num_list h)) = 0

(** [find_card nums h] finds all the cards in [h] that have the numbers [nums].
    Requires: h must have all the cards with numbers in [nums]*)
let rec find_card nums h =
  match nums with
  | [] -> []
  | n :: t -> List.find (fun c -> Card.get_num c = n) h :: find_card t h

(** [find_straight_helper h] is used by [find_straight]. Returns the straight
    combination in [h]. Requires: [h] is sorted*)
let rec find_straight_helper h =
  if List.length (diff [ 10; 11; 12; 13; 1 ] (num_list h)) = 0 then
    find_card [ 10; 11; 12; 13; 1 ] h
  else if List.length (diff [ 9; 10; 11; 12; 13 ] (num_list h)) = 0 then
    find_card [ 9; 10; 11; 12; 13 ] h
  else if List.length (diff [ 8; 9; 10; 11; 12 ] (num_list h)) = 0 then
    find_card [ 8; 9; 10; 11; 12 ] h
  else if List.length (diff [ 7; 8; 9; 10; 11 ] (num_list h)) = 0 then
    find_card [ 7; 8; 9; 10; 11 ] h
  else if List.length (diff [ 6; 7; 8; 9; 10 ] (num_list h)) = 0 then
    find_card [ 6; 7; 8; 9; 10 ] h
  else if List.length (diff [ 5; 6; 7; 8; 9 ] (num_list h)) = 0 then
    find_card [ 5; 6; 7; 8; 9 ] h
  else if List.length (diff [ 4; 5; 6; 7; 8 ] (num_list h)) = 0 then
    find_card [ 4; 5; 6; 7; 8 ] h
  else if List.length (diff [ 3; 4; 5; 6; 7 ] (num_list h)) = 0 then
    find_card [ 3; 4; 5; 6; 7 ] h
  else if List.length (diff [ 2; 3; 4; 5; 6 ] (num_list h)) = 0 then
    find_card [ 2; 3; 4; 5; 6 ] h
  else if List.length (diff [ 1; 2; 3; 4; 5 ] (num_list h)) = 0 then
    find_card [ 1; 2; 3; 4; 5 ] h
  else failwith "not a straight"

(** [find_straight h] finds the straight cards in [h]*)
let rec find_straight h = find_straight_helper (sort_cards_uniq h)

let flush h =
  let res =
    List.fold_right
      (fun s acc ->
        let f = List.filter (fun c -> Card.get_suit c = s) h in
        (List.length f >= 5) :: acc)
      [ 'S'; 'D'; 'H'; 'C' ] []
  in
  List.exists (fun b -> b) res

(** [find_flush h] finds the flush card combination in [h]. Requires: [h] must
    have a flush card combination in it*)
let find_flush h =
  let sorted_list = sort_cards h in
  let s_list =
    List.filter
      (fun c -> if Card.get_suit c = 'S' then true else false)
      sorted_list
  in
  if List.length s_list >= 5 then get_n s_list 5
  else
    let c_list =
      List.filter
        (fun c -> if Card.get_suit c = 'C' then true else false)
        sorted_list
    in
    if List.length c_list >= 5 then get_n c_list 5
    else
      let h_list =
        List.filter
          (fun c -> if Card.get_suit c = 'H' then true else false)
          sorted_list
      in
      if List.length h_list >= 5 then get_n h_list 5
      else
        let d_list =
          List.filter
            (fun c -> if Card.get_suit c = 'D' then true else false)
            sorted_list
        in
        if List.length d_list >= 5 then get_n d_list 5 else failwith "no flush"

let full_house h =
  if is_n_kind 3 h then
    let t = find_n_kind 3 h in
    let no_t = remove_num (Card.get_num (List.hd t)) h in
    pair no_t
  else false

let four_of_a_kind h = is_n_kind 4 h
let straight_flush h = if flush h then straight (find_flush h) else false

let royal_flush h =
  flush h
  && List.length (diff [ 10; 11; 12; 13; 1 ] (num_list (find_flush h))) = 0

(** [get_pair_hand h] gets the best 5 card combination of a hand with a pair.
    Requires: [h] must be a pair combination*)
let get_pair_hand h =
  let pair_hand = find_n_kind 2 h in
  match pair_hand with
  | [ c1; c2 ] ->
      let removed_pair_lst = remove_num (Card.get_num c1) h in

      (2, c1 :: c2 :: get_n (sort_cards removed_pair_lst) 3)
  | _ -> failwith "Not a pair"

(** [get_two_pair_hand h] gets the best 5 card combination of a hand with two
    pairs. Requires: [h] must be a two pair combination*)
let get_two_pair_hand h =
  let sorted_list = sort_cards h in
  let p1 = find_n_kind 2 sorted_list in
  let no_p1 = remove_num (Card.get_num (List.hd p1)) sorted_list in
  let p2 = find_n_kind 2 no_p1 in
  let no_p2 = remove_num (Card.get_num (List.hd p2)) no_p1 in
  (3, p1 @ p2 @ [ List.hd no_p2 ])

(** [get_four_kind_hand h] gets the best 5 card combination of a hand with four
    of a kind. Requires: [h] must be a four of a kind combination*)
let get_four_kind_hand h =
  let f = find_n_kind 4 h in
  let sorted_h = sort_cards h in
  let no_f = remove_num (Card.get_num (List.hd f)) sorted_h in
  (8, f @ [ List.hd no_f ])

(** [get_three_kind_hand h] gets the best 5 card combination of a hand with
    three of a kind. Requires: [h] must be a three of a kind combination*)
let get_three_kind_hand h =
  let t = find_n_kind 3 h in
  let sorted_h = sort_cards h in
  let no_t = remove_num (Card.get_num (List.hd t)) sorted_h in
  match no_t with
  | c1 :: c2 :: _ -> (4, t @ [ c1; c2 ])
  | _ -> failwith "not three of a kind"

(** [get_full_house_hand h] gets the best 5 card combination of a hand with a
    full house. Requires: [h] must be a full house combination*)
let get_full_house_hand h =
  let t = find_n_kind 3 h in
  let sorted_h = sort_cards h in
  let no_t = remove_num (Card.get_num (List.hd t)) sorted_h in
  let p = find_n_kind 2 no_t in
  (7, p @ t)

let get_rank (h : Card.t list) : int * Card.t list =
  if royal_flush h then (10, find_flush h)
  else if straight_flush h then (9, find_flush h)
  else if four_of_a_kind h then get_four_kind_hand h
  else if full_house h then get_full_house_hand h
  else if flush h then (6, find_flush h)
  else if straight h then (5, find_straight h)
  else if three_of_a_kind h then get_three_kind_hand h
  else if two_pair h then get_two_pair_hand h
  else if pair h then get_pair_hand h
  else (1, get_n (sort_cards h) 5)
