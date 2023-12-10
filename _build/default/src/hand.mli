(** Representation of a hand of cards.

    This module represents a hand of poker cards.

    This module can draw a random hand of cards from a deck, and it can also
    identify the hand's ranking. *)

type t
(** The abstract type of values representing a hand of cards. *)

val draw_hand : int -> Deck.t -> t
(** [draw_hand i d] initializes a hand with i number of random cards from deck
    d. *)

val get_n : t -> int -> Card.t list
(** [get_n hand n] is the first [n] cards of [hand]. Requires: [hand] has at
    least [n] cards*)

val make_hand : Card.t list -> t
(**[make_hand l] makes a hand out of a list l of cards*)

val hand_size : t -> int
(**[hand_size t] returns the number of cards in t as an int*)

val get_hand : t -> Card.t list
(** [get_cards h] returns a list of the cards in hand h.*)

val get_rank : t -> int * Card.t list
(** [get_rank h] returns the rank of the hand h as an int, with 1 being the
    highest (royal flush).*)

val add_hands : t -> t -> t
(** [add_hands h1 h2] combines the hands [h1] and [h2] *)

val pair : Card.t list -> bool
(** [pair h] returns true if there is a pair in the hand. *)

val two_pair : Card.t list -> bool
(** [two_pair h] returns true if there is a two pair in the hand. *)

val three_of_a_kind : Card.t list -> bool
(** [three_of_a_kind h] returns true if there is a three of a kind in the hand. *)

val straight : Card.t list -> bool
(** [straight h] returns true if there is a straight in the hand. *)

val flush : Card.t list -> bool
(** [flush h] returns true if there is a flush in the hand. *)

val full_house : Card.t list -> bool
(** [full_house h] returns true if there is a full house in the hand. *)

val four_of_a_kind : Card.t list -> bool
(** [four_of_a_kind h] returns true if there is a four of a kind in the hand. *)

val straight_flush : Card.t list -> bool
(** [straight_flush h] returns true if there is a straight flush in the hand. *)

val royal_flush : Card.t list -> bool
(** [royal_flush h] returns true if there is a royal flush in the hand. *)
