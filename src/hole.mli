(** Representation of a hole.

    This module represents a hole make up of 2 poker cards.

    You can draw a random hole, retrieve the first and second cards in the hole,
    and determine if the hole is a pair in this module. *)

type t
(** The abstract type of values representing the two card hole. *)

exception IllegalHole
(** Raised when a hole is an invalid form*)

val make_hole : Deck.t -> t
(** [make_hole] randomly draws two cards from the deck. *)

val make_hole_cards : Card.t list -> t
(** [make_hole cards] creates a hole with [cards]*)

val get_first : t -> Card.t
(** [get_first t] returns first card in the hole. *)

val get_second : t -> Card.t
(** [get_second t] returns second card in the hole. *)

val get_cards : t -> Card.t list
(** [get_cards t returns the hole of player as a list of cards]*)

val is_pair : t -> bool
(** [is_pair t] returns True is the hole is a pair. *)
