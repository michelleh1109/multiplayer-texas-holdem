open Command
open Hole

type t = {
  mutable balance : int;
  mutable hole : Hole.t;
  name : string;
  mutable owes : int;
}
(* RI : balance cannot be <= 0*)

exception IllegalPlayer

let make_player deck amt n =
  let h = Hole.make_hole deck in
  { balance = amt; hole = h; name = n; owes = 0 }

let make_player_hole cards n =
  { balance = 0; hole = make_hole_cards cards; name = n; owes = 0 }

let get_balance p = p.balance
let get_first p = Hole.get_first p.hole
let get_second p = Hole.get_second p.hole
let get_hole p = p.hole
let get_name t = t.name
let get_owes t = t.owes

exception CheckFailed of int
exception RaiseFailed of int * int * int
exception CallFailed of int
exception AllInFailed

let make_move (command : command) (p1 : t) (p2 : t) =
  match command with
  | Fold -> p1.owes <- 0
  | Check ->
      let amnt = p1.owes in
      if amnt <> 0 then raise (CheckFailed amnt) else ()
  | Raise n ->
      let amnt = p1.owes in
      if amnt > n || p1.balance < n then
        raise (RaiseFailed (p1.balance, amnt, n))
      else (
        p1.balance <- p1.balance - n;
        if p2.balance = 0 then p2.owes <- 0 else p2.owes <- n - p1.owes;
        p1.owes <- 0)
  | Call ->
      if p1.owes = 0 then raise (CallFailed p1.owes)
      else if p1.owes > p1.balance then raise (CallFailed p1.owes)
      else p1.balance <- p1.balance - p1.owes;
      p1.owes <- 0
  | AllIn ->
      if p1.balance = 0 then raise AllInFailed
      else (
        if p1.balance > p1.owes && p2.balance <> 0 then
          p2.owes <- p1.balance - p1.owes
        else p2.owes <- 0;
        p1.owes <- 0;
        p1.balance <- 0)
  | _ -> failwith "anything else shouldn't call make move"
