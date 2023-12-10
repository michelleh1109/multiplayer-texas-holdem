open Player

type game = {
  mutable round : int;
  mutable comp_raised : bool;
  hand : Hand.t;
  mutable game_ended : bool * Player.t;
}

(** [check_print comp] is what is printed when the computer plays check*)
let check_print () = print_endline "the computer checks"

(** [fold_print comp] is what is printed when the computer plays fold*)
let fold_print () = print_endline "the computer folds"

(** [call_print comp] is what is printed when the computer plays call*)
let call_print (comp : Player.t) =
  print_endline
    ("The computer calls. The computer now has $"
    ^ string_of_int (Player.get_balance comp))

(** [all_in_print comp] is what is printed when the computer plays all in*)
let all_in_print (comp : Player.t) =
  print_endline
    ("The computer goes all in. Now it has $"
    ^ string_of_int (Player.get_balance comp))

(** [raise_print comp] is what is printed when the computer plays raise*)
let raise_print (r : int) (comp : Player.t) =
  let balance = Player.get_balance comp in
  print_endline
    ("The computer raises by $" ^ string_of_int r ^ ". Now it has $"
    ^ string_of_int (balance - r))

(** [computer_turn_easy] what the computer with difficuly easy does when its its
    turn*)
let computer_turn_easy (comp : Player.t) (p : Player.t) : unit =
  let owes = Player.get_owes comp in
  let balance = Player.get_balance comp in
  match (balance, owes) with
  | 0, _ ->
      Player.make_move Check comp p;
      check_print ()
  | b, 0 ->
      Player.make_move Check comp p;
      check_print ()
  | b, o when b > o ->
      Player.make_move Call comp p;
      call_print comp
  | b, o when o >= b ->
      Player.make_move AllIn comp p;
      all_in_print comp
  | _ -> failwith "not implemented"

(** [computer_turn_medium] what the computer with difficuly medium does when its
    its turn*)
let computer_turn_medium (comp : Player.t) (p : Player.t) (game_state : game) :
    unit =
  let owes = Player.get_owes comp in
  let balance = Player.get_balance comp in
  if game_state.comp_raised = true then (
    game_state.comp_raised <- false;
    match (balance, owes) with
    | _, 0 ->
        check_print ();
        Player.make_move Check comp p
    | b, o when o < b ->
        Player.make_move Call comp p;
        call_print comp
    | b, o ->
        Player.make_move AllIn comp p;
        all_in_print comp)
  else
    match (balance, owes) with
    | 0, _ ->
        Player.make_move Check comp p;
        check_print ()
    | b, o when o = 0 ->
        let rand_act = Random.int 100 in
        if rand_act < 30 then (
          Player.make_move Check comp p;
          check_print ())
        else if rand_act < 95 then (
          let r = if b > 100 then Random.int 30 else Random.int b in
          raise_print r comp;
          Player.make_move (Raise r) comp p;
          game_state.comp_raised <- true)
        else (
          Player.make_move AllIn comp p;
          all_in_print comp)
    | b, o ->
        if o >= b then (
          Player.make_move AllIn comp p;
          all_in_print comp)
        else if b - o < 50 then (
          call_print comp;
          Player.make_move Call comp p)
        else
          let r = o + Random.int 30 in
          raise_print r comp;
          Player.make_move (Raise r) comp p;
          game_state.comp_raised <- true

let computer_turn (comp : Player.t) (p : Player.t) (diff : string ref)
    (game_state : game) : unit =
  if !diff = "easy" then computer_turn_easy comp p
  else computer_turn_medium comp p game_state
