open Card
open Deck
open Hole

type terminal_card = {
  line1 : string;
  line2 : string;
  line3 : string;
  line4 : string;
}

(** [empty_ter] corresponds to an empty card, meaning no cards in the terminal*)
let empty_ter = { line1 = ""; line2 = ""; line3 = ""; line4 = "" }

(** [suit_to_symbol s] converts the suit [s] to its symbolic representation.
    Raises: IllegalSuit if [s] is not a valid suit*)
let suit_to_symbol s =
  match s with
  | 'D' -> "♦"
  | 'S' -> "♠"
  | 'H' -> "♥"
  | 'C' -> "♣"
  | _ -> raise IllegalSuit

(** [str_num n] converts card number [n] to its stringified version. If [n] is
    less than 11, then it simply outputs the string version of n. If it's a
    special card, then it outputs the symbol of the special card. For example 11
    is J. 12 is Q. 13 is K Raises: IllegalNum if [n] is not a valid number*)
let str_num n =
  if n > 0 && n < 11 then string_of_int n
  else
    match n with
    | 11 -> "J"
    | 12 -> "Q"
    | 13 -> "K"
    | _ -> raise IllegalNum

(* [update_ter card ter spc] updates ter by adding card to [ter]. [spc] is the
   spacing between cards*)
let update_ter card ter spc =
  let num = str_num (get_num card) in
  let suit = suit_to_symbol (get_suit card) in
  if num = "10" then
    let line1 = ter.line1 ^ spc ^ num ^ " ─── " in
    let line2 = ter.line2 ^ spc ^ "│     │" in
    let line3 = ter.line3 ^ spc ^ "│  " ^ suit ^ "  │" in
    let line4 = ter.line4 ^ spc ^ " ─── " ^ num in
    { line1; line2; line3; line4 }
  else
    let line1 = ter.line1 ^ spc ^ num ^ " ──── " in
    let line2 = ter.line2 ^ spc ^ "│     │" in
    let line3 = ter.line3 ^ spc ^ "│  " ^ suit ^ "  │" in
    let line4 = ter.line4 ^ spc ^ " ──── " ^ num in
    { line1; line2; line3; line4 }

(* [print_ter ter] prints [ter] onto the terminal. [ter] represents what each
   line contains. A card makes up 5 lines*)
let print_ter { line1; line2; line3; line4 } =
  let output =
    "\n" ^ line1 ^ "\n" ^ line2 ^ "\n" ^ line3 ^ "\n" ^ line2 ^ "\n" ^ line4
    ^ "\n"
  in
  print_endline output

(** [cards_to_ter cards] converts the list of [cards] to [ter]. [ter] represents
    each line of the terminal where the cards are in one line*)
let rec cards_to_ter cards =
  match cards with
  | [] -> empty_ter
  | c :: t -> update_ter c (cards_to_ter t) " "

and print_cards cards = print_ter (cards_to_ter cards)

let print_card card = print_cards [ card ]

let print_hand hand =
  let cards = [ get_first hand; get_second hand ] in
  print_cards cards
