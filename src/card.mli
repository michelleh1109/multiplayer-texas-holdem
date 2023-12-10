(** Representation of card data.

    This module represents poker cards, including cards of suit Hearts,
    Diamonds, Spades, and Clubs and numbers 1 through 13. Number 11 represents
    Jack, 12 Queen, and 13 King.

    This module can create a card or list of cards, retrieve card data, and even
    make a string version of the card. *)

type t
(** The abstract type of values representing cards. *)

exception IllegalNum
(** Raised when a number cannot be represented by a card*)

exception IllegalSuit
(** Raised when a character does not correspond to a suit*)

val make_card : int -> char -> t
(** [make_card num suit] is the card with [num] and [suit]. Requires: 1 <= [num]
    <= 13, and suit one of S, H, D, or S. Raises: IllegalNum if [num] is not a
    valid number. IllegalSuit if [suit] is not a valid suit*)

val get_num : t -> int
(** [get_num card] is the number the card is representing. *)

val get_suit : t -> char
(** [get_suit card] is the suit the card is representing.*)

val str_card : t -> string
(** [str_card card] is the string version of card. Ex: 5 of Spades. Queen of
    Hearts. Ace of Clubs. King of Diamonds*)

val create_card_list : (int * char) list -> t list
(** [create_card_list] creates a list of cards using [spec]. For example, if
    [spec] is [(1, 'H'); (2, 'D')], then it creates a list that has the cards
    ace of hearts and two of diamonds*)

val card_list_equal : t list -> t list -> bool
(** [card_list_equal c1 c2] is true if c1 is equal to c2*)
