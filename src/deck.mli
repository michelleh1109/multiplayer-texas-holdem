(** Representation of a deck of cards.

    This module represents a deck of poker cards from the card module.

    This module can create a full deck of 52 cards or an empty deck of 0 cards.
    It can retrieve the list of cards in the deck and the size of the deck, as
    well as draw a ramdom card from the deck or remove a specified card. *)

type t
(** The abstract type of values representing the deck. *)

exception IllegalCard
(** Raised when a card is not in the deck*)

val init_deck : unit -> t
(** [init_deck] initializes a deck with all 52 cards. *)

val empty_deck : unit -> t
(** [empty_deck] initializes an empty deck with no cards. *)

val get_cards : t -> Card.t list
(** [get_cards deck] returns the cards currently in the deck. *)

val get_size : t -> int
(** [get_size deck] is the number of cards the deck currently has. *)

val remove_card : t -> Card.t -> t
(** [remove_card deck card] removes a given card from the deck.*)

val draw_card : t -> Card.t
(**[draw_card deck] evaluates to a random card in deck and removes that card
   from deck*)
