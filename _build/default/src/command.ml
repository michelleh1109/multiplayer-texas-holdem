type command =
  | Fold
  | Check
  | Raise of int
  | Call
  | AllIn
  | Help
  | Pot
  | Balance
  | Hand
  | Hole

type diff_command =
  | Easy
  | Medium

type start_command =
  | Start
  | Tutorial
  | Multiplayer

exception Empty
exception Malformed

let rec remove_spaces lst =
  match lst with
  | [] -> []
  | h :: t -> if h = "" then remove_spaces t else h :: remove_spaces t

let parse str =
  let words_only = remove_spaces (String.split_on_char ' ' str) in
  match words_only with
  | [] -> raise Empty
  | "fold" :: _ -> Fold
  | "check" :: _ -> Check
  | "raise" :: i :: _ -> (
      try Raise (int_of_string i) with _ -> raise Malformed)
  | "call" :: _ -> Call
  | "all" :: "in" :: _ -> AllIn
  | "help" :: _ -> Help
  | "pot" :: _ -> Pot
  | "balance" :: _ -> Balance
  | "hand" :: _ -> Hand
  | "hole" :: _ -> Hole
  | _ -> raise Malformed

let parse_diff str =
  let words_only = remove_spaces (String.split_on_char ' ' str) in
  match words_only with
  | [] -> raise Empty
  | "easy" :: _ -> Easy
  | "medium" :: _ -> Medium
  | _ -> raise Malformed

let parse_start str =
  let words_only = remove_spaces (String.split_on_char ' ' str) in
  match words_only with
  | [] -> raise Empty
  | "play" :: _ -> Start
  | "tutorial" :: _ -> Tutorial
  | "multiplayer" :: _ -> Multiplayer
  | _ -> raise Malformed
