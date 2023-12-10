open Poker
open Print
open Deck
open Card
open Hole
open Command
open Hand
open Player
open Unix
open Computer

let _ = Random.self_init ()
let deck = Deck.init_deck ()
let difficulty = ref "easy"
let player1 = make_player deck 250 "Player1"
let computer1 = make_player deck 250 "Computer"
let tut_deck = Deck.init_deck ()
let mult_deck = Deck.init_deck ()
let tut_player1 = make_player deck 250 "Player1"
let tut_computer1 = make_player deck 250 "Computer"
let mult_p1 = make_player mult_deck 250 "Player 1"
let mult_p2 = make_player mult_deck 250 "Player 2"

let game_state =
  {
    round = 1;
    comp_raised = false;
    hand = draw_hand 5 deck;
    game_ended = (false, player1);
  }

let tut_game_state =
  {
    round = 1;
    comp_raised = false;
    hand = draw_hand 5 tut_deck;
    game_ended = (false, tut_player1);
  }

let mult_game_state =
  {
    round = 1;
    comp_raised = false;
    hand = draw_hand 5 mult_deck;
    game_ended = (false, mult_p1);
  }

(** [welcome_message ()] is the introductory message to the user and shows the
    user their hand*)
let welcome_message () =
  print_endline
    "\n\n\
     Howdy partner, welcome to our Texas Hold'em game! Are y'all ready to \
     wrangle up some cards and show us what you're made of? Saddle up and get \
     ready for a rootin' tootin' good time. Don't forget to hold onto your hat \
     and enjoy the ride! Good luck, and may the cards be in your favor!";
  sleep 1

(** [switch_computer s] is called during the multiplayer mode, when players are
    switching computers*)
let switch_computer s =
  print_endline (s ^ " it is your turn to play. Once you are ready, press enter");
  let _ = read_line () in
  ()

(** [enter_pause ()] pauses until enter is hit by the user*)
let enter_pause () =
  print_endline "Press enter to continue";
  let _ = read_line () in
  ()

(** [print_poker_hands ()] prints all the possible poker hands for demonstration
    purposes*)
let print_poker_hands () =
  print_endline "Here are the rankings of different combinations of cards:";
  let royal_flush =
    create_card_list
      (List.rev [ (10, 'H'); (11, 'H'); (12, 'H'); (13, 'H'); (1, 'H') ])
  in
  enter_pause ();
  print_endline "1) Royal Flush: ";
  print_cards royal_flush;
  let straight_flush =
    create_card_list
      (List.rev [ (3, 'S'); (4, 'S'); (5, 'S'); (6, 'S'); (7, 'S') ])
  in
  enter_pause ();
  print_endline "2) Straight Flush: ";
  print_cards straight_flush;
  let four_kind =
    create_card_list
      (List.rev [ (12, 'H'); (12, 'D'); (12, 'S'); (12, 'C'); (2, 'D') ])
  in
  enter_pause ();
  print_endline "3) Four of a Kind: ";
  print_cards four_kind;
  let full_house =
    create_card_list
      (List.rev [ (9, 'D'); (9, 'H'); (9, 'S'); (5, 'S'); (5, 'H') ])
  in
  enter_pause ();
  print_endline "4) Full House: ";
  print_cards full_house;
  let flush =
    create_card_list
      (List.rev [ (10, 'S'); (3, 'S'); (6, 'S'); (11, 'S'); (7, 'S') ])
  in
  enter_pause ();
  print_endline "5) Flush: ";
  print_cards flush;
  let straight =
    create_card_list
      (List.rev [ (5, 'S'); (6, 'H'); (7, 'S'); (8, 'D'); (9, 'C') ])
  in
  enter_pause ();
  print_endline "6) Straight: ";
  print_cards straight;
  let three_kind =
    create_card_list
      (List.rev [ (10, 'D'); (10, 'S'); (10, 'C'); (7, 'D'); (2, 'C') ])
  in
  enter_pause ();
  print_endline "7) Three of a Kind:";
  print_cards three_kind;
  let two_pair =
    create_card_list
      (List.rev [ (9, 'H'); (9, 'S'); (5, 'C'); (5, 'S'); (11, 'D') ])
  in
  enter_pause ();
  print_endline "8) Two Pairs";
  print_cards two_pair;
  let pair =
    create_card_list
      (List.rev [ (12, 'H'); (12, 'S'); (1, 'H'); (2, 'C'); (7, 'S') ])
  in
  enter_pause ();
  print_endline "9) One Pair";
  print_cards pair;
  let high_card =
    create_card_list
      (List.rev [ (12, 'H'); (11, 'C'); (7, 'S'); (5, 'S'); (2, 'D') ])
  in
  enter_pause ();
  print_endline "10) High Card";
  print_cards high_card

(** [show_hand ()] shows the user their cards*)
let show_hand () =
  print_endline "\nHere is your hand:";
  print_hand (get_hole player1);
  sleepf 0.5

(** [diff_empty_seq ()] tells the user to not enter an empty difficulty*)
let rec diff_empty_seq () =
  print_endline
    "\nYou cannot enter an empty difficulty. Please enter either easy or medium";
  difficulty_logic (read_line ())

(** [diff_malformed_seq ()] tells the user to not enter an invalid difficulty*)
and diff_malformed_seq () =
  print_endline
    "\n This is not a valid difficulty. Please enter either easy or medium ";
  difficulty_logic (read_line ())

(** [difficulty_logic input] outputs what difficulty the user picks. If the user
    input is malformed or empty, calls the appropriate functions*)
and difficulty_logic input : diff_command =
  try parse_diff input with
  | Empty -> diff_empty_seq ()
  | Malformed -> diff_malformed_seq ()

(** [set_difficulty ()] sets the computer difficulty based on user input*)
and set_difficulty () =
  print_endline
    "Before playing, please select a difficulty level. You can type either \
     easy or medium ";
  match difficulty_logic (read_line ()) with
  | Easy -> difficulty := "easy"
  | Medium -> difficulty := "medium"

(** [computer_turn comp p] is what the computer does in its turn, and prints the
    computers action*)
let computer_turn comp p state =
  print_endline "\nIt is the computer's turn: \n";
  sleepf 0.5;
  computer_turn comp p difficulty state

(** [print_pot ()] prints the current balance in the pot*)
let print_pot p1 p2 =
  let pot = 250 - Player.get_balance p1 + (250 - Player.get_balance p2) in
  print_endline ("Currently, there is $" ^ string_of_int pot);
  print_endline "Please make a move:"

(** [print_balance player] prints the balance of [player]*)
let print_balance player =
  print_endline
    (get_name player ^ " has $" ^ string_of_int (get_balance player) ^ " left")

(** [print_hole player] prints the hole of [player]*)
let print_hole player =
  print_endline (Player.get_name player ^ ", here is your hole:");
  print_hand (Player.get_hole player)

(** [print_middle state] prints the current cards in the middle if possible]*)
let print_middle state =
  if state.round = 1 then
    print_endline "There aren't any cards in the middle yet"
  else (
    print_endline "Here are the cards in the middle";
    print_cards (get_n state.hand (state.round + 1)))

(** [malformed_command ()] is called when a command raises Malformed. It asks
    the user to enter a command again*)
let rec malformed_command () : command =
  print_endline
    "This is not a valid command. Please enter a valid command. You can type \
     help for help";
  command_logic (read_line ())

(** [empty_command ()] is called when a comman raises Empty. It asks the user to
    enter a command again*)
and empty_command () : command =
  print_endline "You cannot enter an empty command. Please enter a command:";
  command_logic (read_line ())

(** [command_logic input] outputs what command the user does. If the user input
    is malformed or empty, calls the appropriate functions*)
and command_logic input : command =
  try parse input with
  | Malformed -> malformed_command ()
  | Empty -> empty_command ()

(** [round_logic p1 p2 mult] is the logic that is called every round. It asks
    for user input, and calls the computer move based on input, prints [n] of
    the cards in hand, and advances to [next_round]*)
and round_logic p1 p2 state mult =
  let action = command_logic (read_line ()) in
  match action with
  | Fold ->
      Player.make_move Fold p1 p2;
      if mult then (
        ANSITerminal.erase Screen;
        print_endline (get_name p1 ^ " folds"))
      else ();
      state.game_ended <- (true, p2)
  | Pot ->
      print_pot p1 p2;
      round_logic p1 p2 state mult
  | Balance ->
      print_balance p1;
      print_endline "Please make a move:";
      round_logic p1 p2 state mult
  | Hand ->
      print_middle state;
      print_endline "Please make a move:";
      round_logic p1 p2 state mult
  | Hole ->
      print_hole p1;
      print_endline "Please make a move:";
      round_logic p1 p2 state mult
  | Help ->
      print_endline "\nHere are a list of the valid commands:";
      print_endline
        "check: You don't put in any money to the pot and skip your turn. You \
         can only do this if you don't owe any money";
      print_endline
        "raise <amount>: Raises the amount of money in the pot. If you owe \
         money, you must at least raise the money you owe";
      print_endline "all in: Put all your balance into the pot";

      print_endline "call: Match your opponent's raise";
      print_endline "fold: Give up";
      print_endline "pot: Check the amount of money in the pot";
      print_endline "balance: Check the amount of money you have left";
      print_endline "hole: Check the two cards you started with";
      print_endline "hand: Check the cards in the middle";
      print_endline "Please make a move:";
      round_logic p1 p2 state mult
  | _ -> (
      try
        let _ = Player.make_move action p1 p2 in
        if mult then
          match action with
          | Raise r ->
              ANSITerminal.erase Screen;
              print_endline (get_name p1 ^ " raises by $" ^ string_of_int r);
              print_endline
                ("Now they have $" ^ string_of_int (Player.get_balance p1))
          | Check ->
              ANSITerminal.erase Screen;
              print_endline (get_name p1 ^ " checks");
              print_endline
                ("Now they have $" ^ string_of_int (Player.get_balance p1))
          | Call ->
              ANSITerminal.erase Screen;
              print_endline (get_name p1 ^ " calls");
              print_endline
                ("Now they have $" ^ string_of_int (Player.get_balance p1))
          | AllIn ->
              ANSITerminal.erase Screen;
              print_endline (get_name p1 ^ " goes all in");
              print_endline
                ("Now they have $" ^ string_of_int (Player.get_balance p1))
          | _ -> failwith "no other action should come here"
        else ()
      with
      | RaiseFailed (balance, owes, r) ->
          if balance < r then
            print_endline
              ("You can't raise $" ^ string_of_int r ^ " as you only have $"
             ^ string_of_int balance ^ ". Please make a valid move: ")
          else
            print_endline
              ("You can't raise $" ^ string_of_int r
             ^ " as you need to raise at least $" ^ string_of_int owes
             ^ ". Please make a valid move: ");
          round_logic p1 p2 state mult
      | CallFailed owes ->
          if owes = 0 then
            print_endline
              "You can't call as you don't owe any money. Please enter a valid \
               command: "
          else
            print_endline
              ("You can't raise call as you owe"
              ^ string_of_int (Player.get_balance p1)
              ^ ", but you only have $"
              ^ string_of_int (Player.get_balance p2)
              ^ ". Please make a valid move: ");
          round_logic p1 p2 state mult
      | AllInFailed ->
          print_endline
            "You cannot go all in as you have $0. Please make a valid move: ";
          round_logic p1 p2 state mult
      | CheckFailed owes ->
          print_endline
            ("You cannot check as you owe $" ^ string_of_int owes
           ^ ". Please make a valid move: ");
          round_logic p1 p2 state mult)

(** [round_end n] is called at the end of each round*)
and round_end (n : int) (next : unit -> unit) =
  sleepf 0.5;
  print_endline ("\nYou now have $" ^ string_of_int (Player.get_balance player1));
  sleepf 0.5;
  computer_turn computer1 player1 game_state;
  sleepf 0.5;
  raise_repeat ();
  sleepf 0.5;

  let ended, winner = game_state.game_ended in
  if ended then game_end winner ()
  else (
    if n <> 0 then print_cards (get_n game_state.hand n) else ();
    next ())

(** [raise_repeat p1 p2] is called when the round doesn't end after the computer
    plays because the computer raises*)
and raise_repeat () =
  if Player.get_owes player1 <> 0 then (
    print_endline "It is your turn: ";
    round_logic player1 computer1 game_state false;
    let ended, _ = game_state.game_ended in
    if ended then ()
    else (
      print_endline
        ("\nYou now have $" ^ string_of_int (Player.get_balance player1));
      if Player.get_owes computer1 <> 0 then (
        computer_turn computer1 player1 game_state;
        raise_repeat ())
      else ()))
  else ()

(** [round1 ()] is the first part of the game, up to the point where three cards
    get flipped*)
and round1 () =
  print_endline "You go first. It is your turn to play:";
  round_logic player1 computer1 game_state false;
  let ended, winner = game_state.game_ended in
  if ended then game_end winner () else round_end 3 round2

(** [round2 ()] is the first part of the game, up to the point where the fourth
    card gets flipped*)
and round2 () =
  game_state.round <- 2;
  print_endline "It is your turn to play: ";
  round_logic player1 computer1 game_state false;
  let ended, winner = game_state.game_ended in
  if ended then game_end winner () else round_end 4 round3

(** [round3 ()] is the first part of the game, up to the point where last card
    (fifth) gets flipped*)
and round3 () =
  game_state.round <- 3;
  print_endline "It is your turn to play: ";
  round_logic player1 computer1 game_state false;
  let ended, winner = game_state.game_ended in
  if ended then game_end winner () else round_end 5 round4

(** [round4 ()] is the last betting round*)
and round4 () =
  game_state.round <- 4;
  print_endline "It is your turn to play: ";
  round_logic player1 computer1 game_state false;
  let ended, winner = game_state.game_ended in
  if ended then game_end winner ()
  else
    round_end 0
      (game_end
         (Compare.find_winner player1 computer1 (get_n game_state.hand 5)))

(** [game_end player ()] displays the winner, the cards of the players , and
    terminates the game. The winner is [player]*)
and game_end player () =
  print_endline
    ("\n" ^ get_name player ^ " wins!\nThis was your opponents cards:");
  print_hand (get_hole computer1);
  sleepf 0.5;
  print_endline "This was your cards:";
  print_hand (get_hole player1);
  sleepf 0.5;
  print_endline "Thanks for playing our game!"

(** [mode_empty_seq ()] tells the user to not enter an empty mode*)
let rec mode_empty_seq () =
  print_endline
    "\n\
     You cannot enter an empty mode. Please enter either play, tutorial, or \
     multiplayer";
  mode_logic (read_line ())

(** [mode_malformed_seq ()] tells the user to not enter an invalid mode*)
and mode_malformed_seq () =
  print_endline
    "\n\
    \ This is not a valid mode. Please enter either play, tutorial, or \
     multiplayer";
  mode_logic (read_line ())

(** [mode_logic input] outputs what mode the user picks. If the user input is
    malformed or empty, calls the appropriate functions*)
and mode_logic input : start_command =
  try parse_start input with
  | Empty -> mode_empty_seq ()
  | Malformed -> mode_malformed_seq ()

(** [tut_check ()] forces the user to type check for tutorial purposes*)
let rec tut_check () =
  let words_only = remove_spaces (String.split_on_char ' ' (read_line ())) in
  match words_only with
  | "check" :: _ -> Player.make_move Check tut_player1 tut_computer1
  | _ ->
      print_endline "Please type in check";
      tut_check ()

(** [tut_balance ()] forces the user to type balance for tutorial purposes*)
let rec tut_balance () =
  let words_only = remove_spaces (String.split_on_char ' ' (read_line ())) in
  match words_only with
  | "balance" :: _ ->
      print_endline
        ("You have $" ^ string_of_int (Player.get_balance tut_player1))
  | _ ->
      print_endline "Please type in balance";
      tut_balance ()

(** [tut_hole ()] forces the user to type hole for tutorial purposes*)
let rec tut_hole () =
  let words_only = remove_spaces (String.split_on_char ' ' (read_line ())) in
  match words_only with
  | "hole" :: _ ->
      print_endline "These are your hole cards";
      print_hand (Player.get_hole tut_player1)
  | _ ->
      print_endline "Please type in hole";
      tut_hole ()

(** [tut_pot ()] forces the user to type pot for tutorial purposes*)
let rec tut_pot () =
  let words_only = remove_spaces (String.split_on_char ' ' (read_line ())) in
  match words_only with
  | "pot" :: _ ->
      let amnt =
        250
        - Player.get_balance tut_player1
        + (250 - Player.get_balance tut_computer1)
      in
      print_endline ("Currently, the pot has $" ^ string_of_int amnt)
  | _ ->
      print_endline "Please type in hole";
      tut_hole ()

(** [tut_raise ()] forces the user to type raise 20 for tutorial purposes*)
let rec tut_raise () =
  let words_only = remove_spaces (String.split_on_char ' ' (read_line ())) in
  match words_only with
  | "raise" :: "20" :: _ ->
      Player.make_move (Raise 20) tut_player1 tut_computer1
  | _ ->
      print_endline "Please type in raise 20";
      tut_raise ()

(** [pick_mode ()] asks the user to pick either the game or the tutorial*)
let rec pick_mode () =
  print_endline
    "\n\
     Would you like to jump into poker or walk through an interactive \
     tutorial. You can also play with a friend using the same computer? Type \
     'play' or 'tutorial' or 'multiplayer'";
  match mode_logic (read_line ()) with
  | Start -> init_game ()
  | Tutorial -> init_tutorial ()
  | Multiplayer -> init_multiplayer ()

(** [init_game ()] initializes the game*)
and init_game () =
  set_difficulty ();
  show_hand ();
  round1 ()

(** [init_tutorial ()] initializes the tutorial sequence*)
and init_tutorial () =
  print_endline
    "The objective of Texas Hold'em is to make the best possible five-card \
     poker hand using the two cards you start with, which is called a hole";
  enter_pause ();
  print_endline
    "Every player gets dealt two cards. In this game, you are playing against \
     the computer. For tutorial purposes, we will show you everyone's cards.";
  enter_pause ();
  print_endline "Here are your cards:";
  print_hand (Player.get_hole tut_player1);
  print_endline "Here are your oponnent's cards:";
  print_hand (Player.get_hole tut_computer1);
  enter_pause ();
  print_endline "There are five stages to poker: ";
  print_endline
    "Game begins with a betting round. In every betting round, you can either \
     check (skip your turn), raise (bet and increase the money in the pot), \
     call (match your oponnent's raise), or fold (give up).";
  enter_pause ();
  print_endline
    "After the first betting round, the dealer reveals the first three \
     community cards, which is called the flop. Following this, another round \
     of betting begins, where players have the same option as the previous \
     betting round.";
  enter_pause ();
  print_endline
    "Afterwards, the dealer reveals the fourth community card, called the \
     turn, which is followed by another round of betting. After this, the last \
     community card is revealed, called the river, and the final round of \
     betting occurs.";
  enter_pause ();
  print_endline
    "After the final betting round, the players reveal their hole cards, and \
     the best five hand card combination wins.";
  print_poker_hands ();
  print_endline
    "Now that you know the rules, lets walk through an interactive tutorial so \
     that you understand how everything works:";
  enter_pause ();
  print_endline
    "The betting round starts with you. Let's start out with check. Check \
     means you are not putting any money into the pot, and your are skipping \
     your turn to see what your opponent does. Please type check: ";
  tut_check ();
  print_endline "Perfect! Now, let's see what your opponent does:";
  enter_pause ();
  computer_turn tut_computer1 tut_player1 tut_game_state;
  print_endline
    "The computer also checks. Now, the betting round is over, and it is time \
     to flip the first three cards:";
  enter_pause ();
  print_cards (get_n tut_game_state.hand 3);
  print_endline
    "These are the first three cards. Now, it is your turn to play again. \
     Let's try raising $20. Type raise 20: ";
  tut_raise ();
  print_endline "Perfect! Now, let's see what your opponent does:";
  enter_pause ();
  computer_turn tut_computer1 tut_player1 tut_game_state;
  print_endline
    "The computer calls, which means that the computer also raises by $20, \
     matching your raise. Since your computer didn't raise more than you, the \
     betting round is over. Time to flip an additional card:";
  enter_pause ();
  print_cards (get_n tut_game_state.hand 4);
  print_endline "It is your turn again. Check your balance by typing balance";
  tut_balance ();
  print_endline "Check your hole by typing hole";
  tut_hole ();
  print_endline "Check the amount of money in the pot by typing pot";
  tut_pot ();
  print_endline "Now, play check again";
  tut_check ();
  print_endline "Perfect! Now, let's see what your opponent does:";
  computer_turn tut_computer1 tut_player1 tut_game_state;
  print_endline "The computer also checks, lets flip the last round of cards";
  enter_pause ();
  print_cards (get_n tut_game_state.hand 5);
  enter_pause ();
  print_endline
    "After this last round, there is one final round of betting, but I think \
     you get the drill";
  print_endline
    "Throughout the game, if you forget the available commands, you can always \
     type 'help' during your turn to remember them. Thank you for going \
     through the tutorial";
  pick_mode ()

(** [mult_game_logic n] is the logic used to run multiplayer mode*)
and mult_game_logic n =
  switch_computer "Player 1";
  print_endline "Player 1, here is your hand:";
  print_hand (Player.get_hole mult_p1);
  print_endline "It is your turn. Please make a move:";
  round_logic mult_p1 mult_p2 mult_game_state true;
  let ended, p = mult_game_state.game_ended in
  if ended then mult_game_end p
  else (
    switch_computer "Player 2";
    if n > 3 then (
      print_endline "These are the cards in the middle:";
      print_cards (get_n mult_game_state.hand (n - 1)))
    else ();
    print_endline "Player 2, here is your hand:";
    print_hand (Player.get_hole mult_p2);
    print_endline "It is your turn. Please make a move:";
    round_logic mult_p2 mult_p1 mult_game_state true;
    mult_raise_repeat ();
    if n = 6 then
      mult_game_state.game_ended <-
        ( true,
          Compare.find_winner mult_p1 mult_p2 (get_n mult_game_state.hand 5) )
    else ();
    let ended, p = mult_game_state.game_ended in
    if ended then mult_game_end p
    else (
      print_cards (get_n mult_game_state.hand n);
      mult_game_state.round <- n - 1;
      mult_game_logic (n + 1)))

(** [mult_raise_repeat ()] is called when the round doesn't end after player 2
    plays because the player 2 raises*)
and mult_raise_repeat () =
  if Player.get_owes mult_p1 <> 0 then (
    switch_computer "Player 1";
    print_endline "Please make a move: ";
    round_logic mult_p1 mult_p2 mult_game_state true;
    let ended, _ = mult_game_state.game_ended in
    if ended then ()
    else if Player.get_owes mult_p2 <> 0 then (
      switch_computer "Player 2";
      print_endline "Please make a move";
      round_logic mult_p2 mult_p1 mult_game_state true;
      mult_raise_repeat ())
    else ())
  else ()

(** [mult_game_end p] initiates the game end sequence, congratulaying [p]*)
and mult_game_end p = print_endline ("Congratulations! " ^ get_name p ^ " wins!")

(** [init_multiplater ()] initiates the multiplayer mode sequence*)
and init_multiplayer () =
  print_endline
    "\n\
     Welcome to the multiplayer mode! In this mode, you can play against one \
     other person";
  print_endline
    "Throughout the game, we will tell you to switch players and press enter. \
     Please do this to avoid seeing your oponnent's cards";
  print_endline "Player 1, we start with you";
  mult_game_logic 3

let init =
  welcome_message ();
  pick_mode ()
