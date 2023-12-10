(** Representation of player data.

    This module represents the poker player, human or comupter.

    It manages the player's cards, balance in their account, the current round
    of the game and move, and who has won or lost.*)

type t
(** The abstract type representing the player.*)

exception IllegalPlayer
(** Raised when player is in invalid form such as balance drops below 0*)

exception CheckFailed of int
(** Raised when a player attempts to check on their turn but they owe money*)

exception RaiseFailed of int * int * int
(** Raised when a player attempts to raise on their turn but they don't have
    enough to raise or they raise less than the amount they owe*)

exception CallFailed of int
(** Raised when a player attempts to match another player but they don't have
    enough to match*)

exception AllInFailed
(** raised when a player with 0 balance tries to go all in*)

val make_player : Deck.t -> int -> string -> t
(** [make_player] creates a player of type t with balance of buy in amount with
    hole cards from deck and name n. *)

val make_player_hole : Card.t list -> string -> t
(** [make_player cards name] creates a player with hole made of [cards] and
    [name]*)

val get_balance : t -> int
(** [get_balance t] returns the player balance. *)

val get_first : t -> Card.t
(** [get_first t] returns first card in player hole. *)

val get_second : t -> Card.t
(* *[get_second t] returns second card in player hole. *)

val get_hole : t -> Hole.t
(** [get_hole p] returns the hole of the player *)

val get_name : t -> string
(** [get_name t] returns the name of the player*)

val get_owes : t -> int
(** [get_owe t] returns how much [t] owes*)

val make_move : Command.command -> t -> t -> unit
(** [make_move command p1 p2] updates the fields accordingly in p1 and p2 when
    p1 attempts to execute [command]. Raises: [CheckFailed] if a [p1] attempts
    to check but owes money. [RaiseFailed] if [p1] attempts to raise but they
    don't have enough money or owe more than they raised. [CallFailed] if [p1]
    attempts to match what they owe but they don't have enough money*)
