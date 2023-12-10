(** Representation of the computer "player."

    This module represents the computer as it plays poker, and keeps track of
    the current state of the poker game.

    This module determines what move the computer will play against the human
    player when it is the computer's turn. *)

open Player
open Hand

type game = {
  mutable round : int;
  mutable comp_raised : bool;
  hand : Hand.t;
  mutable game_ended : bool * Player.t;
}

(** type representing the state of the game. round is the current round.
    comp_raied is true if the computer raised within the same round. false
    otherwise*)

val computer_turn : Player.t -> Player.t -> string ref -> game -> unit
(** [computer_turn comp p diff] is what the computer does against the user on
    any given round*)
