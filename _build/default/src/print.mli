(** Prints game information.

    This module prints cards to the terminal so that the player can see it.
    Printed cawrds are visually simple and easy to read, maintaining a sleek and
    minimalist aesthetic.

    It can print one card or a list of multiple cards at a time, and even print
    a hand. *)

val print_card : Card.t -> unit
(** [print_card card] prints a visual of the card in the terminal *)

val print_cards : Card.t list -> unit
(** [print_cards cards] prints a visual of all the cards in [cards] in one line*)

val print_hand : Hole.t -> unit
(** [print_hand hand] prints a visual of the hand in the terminal by printing
    all the cards in the hand*)
