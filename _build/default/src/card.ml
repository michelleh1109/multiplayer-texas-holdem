type t = int * char

exception IllegalSuit
exception IllegalNum

(** [check_num n] returns [n] if it is a valid number (from 1 and 13). Raises:
    IllegalNum if [n] is not a valid number*)
let check_num n = if n >= 1 && n <= 13 then n else raise IllegalNum

(** [check_suit s] returns [s] if it is a valid suit (either S, H, D, or C).
    Raises: IllegalSuit if [s] is not a valid suit*)
let check_suit s =
  match s with
  | 'S' -> 'S'
  | 'H' -> 'H'
  | 'D' -> 'D'
  | 'C' -> 'C'
  | _ -> raise IllegalSuit

let make_card num suit = (check_num num, check_suit suit)

let get_num card =
  match card with
  | n, _ -> n

let get_suit card =
  match card with
  | _, s -> s

(** [string_num n] converts n to a string. If n represents a special card like
    Ace, Jack, Queen, or King, then n becomes the corresponding name. Otherwise,
    n is just the string version of the int*)
let string_num n =
  match n with
  | 13 -> "King"
  | 12 -> "Queen"
  | 11 -> "Jack"
  | 1 -> "Ace"
  | num -> string_of_int num

(** [string_suit s] converts n to a string. If n represents a special card like
    Ace, Jack, Queen, or King, then n becomes the corresponding name. Otherwise,
    n is just the string version of the int*)
let string_suit s =
  match s with
  | 'S' -> "Spades"
  | 'H' -> "Hearts"
  | 'D' -> "Diamonds"
  | 'C' -> "Clubs"
  | _ -> raise IllegalSuit

let rec create_card_list (specs : (int * char) list) =
  match specs with
  | [] -> []
  | (n, s) :: t -> make_card n s :: create_card_list t

let str_card (n, s) = string_num n ^ " of " ^ string_suit s

(** [card_exists c card_list] is true if [c] is in [card_list]*)
let rec card_exists c card_list =
  match card_list with
  | [] -> false
  | h :: t ->
      if get_num h = get_num c && get_suit h = get_suit c then true
      else card_exists c t

let card_list_equal c1 c2 =
  if List.length c1 = List.length c2 then
    List.fold_right (fun c b -> card_exists c c2 && b) c1 true
    && List.fold_right (fun c b -> card_exists c c1 && b) c2 true
  else false
