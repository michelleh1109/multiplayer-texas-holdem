val find_winner : Player.t -> Player.t -> Card.t list -> Player.t
(* [find_winner] takes in two players and list of five cards on the table to
   return winning player *)

val compare_hand :
  Player.t -> Player.t -> int -> Card.t list -> Card.t list -> Player.t
(* [compare_hand] helper function for find_winner that compares tied hands to
   return winning player. Takes in two players, the tied rank (1-10 from
   get_rank) and their respective hands *)
