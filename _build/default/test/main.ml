(** Test Plan: Most of our tests utilize glass-box testing in which we wrote
    individual test cases for each function we implemented. We wrote OUnit test
    cases for most larger modules, including Card, Deck, Hand, and Hole. Modules
    that relied on the terminal (such as print) we tested manually using utop
    and/or our poker game in the terminal. We used glass-box testing and wrote
    test cases as we wrote our code so that nearly every line of code was tested
    in an OUnit test. We used bisect as an additional tool to ensure thorough
    testing coverage. We achieved over 90% coverage with every module except for
    player, which achieved around 70% coverage. This is because many of the
    functions in player.ml involved a randomly generated hole, which was
    difficult to test in OUnit. Player functions are used heavily in
    src/main.ml, so we were able to test the player module using the terminal to
    play a game of poker instead of OUnit tests. This approach allowed us to
    write very thorough tests for every module but player, and allowed us to
    test the player module more quickly and effectively than individual OUnit
    tests would allow. *)

open OUnit2
open Poker
open Card
open Deck
open Print
open Hole
open Command
open Hand

(** [card_suit_test name card expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [get_suit card]. *)
let card_suit_test (name : string) (card : Card.t) (expected_output : char) :
    test =
  name >:: fun _ -> assert_equal expected_output (get_suit card)

(** [card_num_test name card expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [get_num card]. *)
let card_num_test (name : string) (card : Card.t) (expected_output : int) : test
    =
  name >:: fun _ -> assert_equal expected_output (get_num card)

(** [card_str_test name card expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [str_card card]. *)
let card_str_test (name : string) (card : Card.t) (expected_output : string) :
    test =
  name >:: fun _ -> assert_equal expected_output (str_card card)

(** [card_list_equal_test name cl1 cl2 expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [card_list_equal cl1 cl2]. *)
let card_list_equal_test (name : string) (cl1 : Card.t list) (cl2 : Card.t list)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (card_list_equal cl1 cl2)

let card_tests =
  [
    card_suit_test "Hearts suit" (make_card 1 'H') 'H';
    card_suit_test "Spades suit" (make_card 1 'S') 'S';
    card_suit_test "Clubs suit" (make_card 1 'C') 'C';
    card_suit_test "Diamonds suit" (make_card 1 'D') 'D';
    card_num_test "Ace number" (make_card 1 'H') 1;
    card_num_test "7 number" (make_card 7 'H') 7;
    card_num_test "Jack number" (make_card 11 'H') 11;
    card_num_test "King number" (make_card 12 'H') 12;
    card_num_test "Queen number" (make_card 13 'H') 13;
    card_str_test "Queen of Hearts" (make_card 12 'H') "Queen of Hearts";
    card_str_test "Ace of Spades" (make_card 1 'S') "Ace of Spades";
    card_str_test "Seven of Clubs" (make_card 7 'C') "7 of Clubs";
    card_str_test "Ten of Diamonds" (make_card 10 'D') "10 of Diamonds";
    card_list_equal_test "Empty lists are equal" [] [] true;
    card_list_equal_test "1 equal card is true"
      [ make_card 1 'H' ]
      [ make_card 1 'H' ]
      true;
    card_list_equal_test "different number card is false"
      [ make_card 1 'H' ]
      [ make_card 2 'H' ]
      false;
    card_list_equal_test "different suit card is false"
      [ make_card 1 'D' ]
      [ make_card 1 'H' ]
      false;
    card_list_equal_test "card list of length 3 that is equal"
      (create_card_list [ (1, 'S'); (5, 'D'); (1, 'H') ])
      (create_card_list [ (5, 'D'); (1, 'S'); (1, 'H') ])
      true;
    card_list_equal_test "card list of length 3 that is not equal"
      (create_card_list [ (1, 'S'); (5, 'C'); (1, 'H') ])
      (create_card_list [ (5, 'D'); (1, 'S'); (1, 'H') ])
      false;
    card_list_equal_test "card list of length 4 and length 3 is not equal"
      (create_card_list [ (1, 'S'); (5, 'S'); (1, 'H'); (6, 'H') ])
      (create_card_list [ (5, 'D'); (1, 'S'); (1, 'H') ])
      false;
    card_list_equal_test "card list of length 3 and length 4 is not equal"
      (create_card_list [ (5, 'D'); (1, 'S'); (1, 'H') ])
      (create_card_list [ (1, 'S'); (5, 'S'); (1, 'H'); (6, 'H') ])
      false;
    ( "make_card raises IllegalSuit" >:: fun _ ->
      assert_raises IllegalSuit (fun () -> make_card 10 'F') );
    ( "make_card raises IllegalSuit" >:: fun _ ->
      assert_raises IllegalSuit (fun () -> make_card 10 'd') );
    ( "make_card raises IllegalNum" >:: fun _ ->
      assert_raises IllegalNum (fun () -> make_card 0 'D') );
    ( "make_card raises IllegalNum" >:: fun _ ->
      assert_raises IllegalNum (fun () -> make_card 17 'D') );
    ( "make_card raises IllegalNum" >:: fun _ ->
      assert_raises IllegalNum (fun () -> make_card ~-3 'D') );
  ]

(** [deck_size_test name deck expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [get_size deck]. *)

let deck_size_test (name : string) (deck : Deck.t) (expected_output : int) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Deck.get_size deck) ~printer:string_of_int

let deck_tests =
  [
    ("Empty deck" >:: fun _ -> assert_equal 0 (get_size (empty_deck ())));
    deck_size_test "full deck" (init_deck ()) 52;
    deck_size_test "minus S1" (remove_card (init_deck ()) (make_card 1 'S')) 51;
    (let sample_deck = init_deck () in
     deck_size_test "get_cards size" sample_deck
       (List.length (Deck.get_cards sample_deck)));
  ]

(** [pair_test name hole] constructs an OUnit test named [name] that asserts the
    quality of the cards in hole [hole] with [is_pair hole]. *)
let pair_test (name : string) (hole : Hole.t) : test =
  name >:: fun _ ->
  assert_equal
    (get_num (get_first hole) = get_num (get_second hole))
    (is_pair hole)

let deck0 = init_deck ()
let hole_tests = [ pair_test "random hole" (make_hole deck0) ]

(** [parse_test name str expected_output] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [parse str]. *)
let parse_test (name : string) (str : string) (expected_output : command) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (parse str) ~printer:(fun a ->
      match a with
      | Fold -> "fold"
      | Check -> "check"
      | Raise i -> "raise " ^ string_of_int i
      | Call -> "call"
      | AllIn -> "all in"
      | Help -> "help"
      | Pot -> "pot"
      | Balance -> "Balance"
      | Hand -> "Hand"
      | Hole -> "Hole")

(** [parse_diff_test name str expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [parse_diff str]. *)
let parse_diff_test (name : string) (str : string)
    (expected_output : diff_command) : test =
  name >:: fun _ ->
  assert_equal expected_output (parse_diff str) ~printer:(fun a ->
      match a with
      | Easy -> "easy"
      | Medium -> "medium")

(** [parse_start_test name str expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [parse_start str]. *)
let parse_start_test (name : string) (str : string)
    (expected_output : start_command) : test =
  name >:: fun _ ->
  assert_equal expected_output (parse_start str) ~printer:(fun a ->
      match a with
      | Start -> "start"
      | Tutorial -> "Tutorial"
      | Multiplayer -> "Multiplayer")

(** [parse_diff_raise_test name exc input] constructs an OUnit test named [name]
    that checks if [parse_diff input] raises [exc] *)
let parse_diff_raise_test (name : string) exc (input : string) =
  name >:: fun _ -> assert_raises exc (fun () -> parse_diff input)

(** [parse_raise_test name exc input] constructs an OUnit test named [name] that
    checks if [parse_raise input] raises [exc] *)
let parse_raise_test (name : string) exc (input : string) =
  name >:: fun _ -> assert_raises exc (fun () -> parse input)

(** [parse_start_raise_test name exc input] constructs an OUnit test named
    [name] that checks if [parse_start input] raises [exc] *)
let parse_start_raise_test (name : string) exc (input : string) =
  name >:: fun _ -> assert_raises exc (fun () -> parse_start input)

let parse_tests =
  [
    parse_test "fold returns Fold" "  fold   " Fold;
    parse_test "check with commands afterwards returns Check" "check  testtest"
      Check;
    parse_test "raise 5 returns Raise 5" "raise 5" (Raise 5);
    parse_test "raise 5 returns Raise 5" "raise 5 adfja;lkj" (Raise 5);
    parse_test "call returns Call" "call" Call;
    parse_test "all in returns AllIn" "all   in" AllIn;
    parse_test "help returns Help" " help test" Help;
    parse_test "pot returns Pot" " pot test" Pot;
    parse_test "balance returns Balance" " balance test" Balance;
    parse_test "hand returns Hand" " hand test" Hand;
    parse_test "hole returns Hole" " hole test" Hole;
    parse_raise_test "empty string raises Empty " Empty "";
    parse_raise_test "raise without input raises Malformed" Malformed "raise";
    parse_raise_test "raise with non-integer input raises Malformed" Malformed
      "raise true";
    parse_raise_test "random input raises Malformed" Malformed "adfafsdfad";
    parse_diff_test "input easy returns Easy" "easy" Easy;
    parse_diff_test "input medium returns Medium" "medium" Medium;
    parse_diff_test
      "input medium with spaces in the front and back returns Medium"
      "    medium    " Medium;
    parse_start_test "input play returns Start" "play" Start;
    parse_start_test "input tutorial returns Tutorial" "tutorial" Tutorial;
    parse_start_test "multiplayer tutorial returns Multiplayer" "multiplayer"
      Multiplayer;
    parse_diff_raise_test "empty string raises empty" Empty "";
    parse_diff_raise_test "an invalid difficulty raises Malformed" Malformed
      "semi-hard";
    parse_start_raise_test "empty string raises empty" Empty "";
    parse_start_raise_test "start raises Malformed" Malformed "start";
  ]

let deck1 = init_deck ()

(** [hand_cards_test name deck i] constructs an OUnit test named [name] that
    asserts the number of elements in [get_hand (draw_hand i deck)] is i. *)
let hand_cards_test (name : string) (deck : Deck.t) (i : int) : test =
  name >:: fun _ ->
  assert_equal i (List.length (Hand.get_hand (draw_hand i deck)))

(** [hand_size_test name hand i] constructs an OUnit test named [name] that
    asserts the value of [hand_size hand] is i. *)
let hand_size_test (name : string) (hand : Hand.t) (i : int) : test =
  name >:: fun _ -> assert_equal i (hand_size hand)

(** [pair_test name input] constructs an OUnit test named [name] that checks if
    there is a pair in the hand *)
let pair_test (name : string) (input : Card.t list) (expected_output : bool) :
    test =
  name >:: fun _ -> assert_equal expected_output (pair input)

(** [two_pair_test name input] constructs an OUnit test named [name] that checks
    if there is a two pair in the hand *)
let two_pair_test (name : string) (input : Card.t list) (expected_output : bool)
    : test =
  name >:: fun _ -> assert_equal expected_output (two_pair input)

(** [three_of_a_kind_test name input] constructs an OUnit test named [name] that
    checks if there is a three of a kind in the hand *)
let three_of_a_kind_test (name : string) (input : Card.t list)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (three_of_a_kind input)

(** [straight_test name input] constructs an OUnit test named [name] that checks
    if there is a straight in the hand *)
let straight_test (name : string) (input : Card.t list) (expected_output : bool)
    : test =
  name >:: fun _ -> assert_equal expected_output (straight input)

(** [flush_test name input] constructs an OUnit test named [name] that checks if
    there is a flush in the hand *)
let flush_test (name : string) (input : Card.t list) (expected_output : bool) :
    test =
  name >:: fun _ -> assert_equal expected_output (flush input)

(** [full_house_test name input] constructs an OUnit test named [name] that
    checks if there is a full house in the hand *)
let full_house_test (name : string) (input : Card.t list)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (full_house input)

(** [four_of_a_kind_test name input] constructs an OUnit test named [name] that
    checks if there is a four of a kind in the hand *)
let four_of_a_kind_test (name : string) (input : Card.t list)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (four_of_a_kind input)

(** [straight_flush_test name input] constructs an OUnit test named [name] that
    checks if there is a straight flush in the hand *)
let straight_flush_test (name : string) (input : Card.t list)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (straight_flush input)

(** [royal_flush_test name input] constructs an OUnit test named [name] that
    checks if there is a royal flush in the hand *)
let royal_flush_test (name : string) (input : Card.t list)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (royal_flush input)

let hand_tests =
  [
    hand_cards_test "no cards" deck1 0;
    hand_cards_test "one card" deck1 1;
    hand_cards_test "5 cards" deck1 5;
    hand_size_test "empty hand" (make_hand []) 0;
    (let c = make_card 1 'S' in
     hand_size_test "one card" (make_hand [ c ]) 1);
    pair_test "pair present"
      [
        make_card 1 'S';
        make_card 1 'D';
        make_card 2 'S';
        make_card 10 'C';
        make_card 7 'H';
        make_card 12 'C';
        make_card 5 'C';
      ]
      true;
    pair_test "no pair present"
      [
        make_card 1 'S';
        make_card 9 'D';
        make_card 2 'S';
        make_card 10 'C';
        make_card 7 'H';
        make_card 12 'C';
        make_card 5 'C';
      ]
      false;
    two_pair_test "pair present, but no two pair"
      [
        make_card 1 'S';
        make_card 1 'D';
        make_card 2 'S';
        make_card 10 'C';
        make_card 7 'H';
        make_card 12 'C';
        make_card 5 'C';
      ]
      false;
    two_pair_test "two pair present"
      [
        make_card 1 'S';
        make_card 1 'C';
        make_card 2 'S';
        make_card 10 'C';
        make_card 7 'H';
        make_card 12 'C';
        make_card 7 'C';
      ]
      true;
    two_pair_test "there's a three pair, whic counts as a two pair present"
      [
        make_card 1 'S';
        make_card 1 'C';
        make_card 2 'S';
        make_card 2 'D';
        make_card 7 'H';
        make_card 12 'C';
        make_card 7 'C';
      ]
      true;
    three_of_a_kind_test "no three of a kind"
      [
        make_card 1 'S';
        make_card 9 'C';
        make_card 2 'S';
        make_card 3 'D';
        make_card 7 'H';
        make_card 12 'C';
        make_card 7 'C';
      ]
      false;
    three_of_a_kind_test "three of a kind present"
      [
        make_card 1 'S';
        make_card 9 'C';
        make_card 7 'S';
        make_card 3 'D';
        make_card 7 'H';
        make_card 12 'C';
        make_card 7 'C';
      ]
      true;
    three_of_a_kind_test "two three of a kind present"
      [
        make_card 1 'S';
        make_card 9 'C';
        make_card 7 'S';
        make_card 3 'D';
        make_card 7 'H';
        make_card 12 'C';
        make_card 7 'C';
      ]
      true;
    three_of_a_kind_test "a three of a kind is present in a full house"
      [
        make_card 10 'H';
        make_card 10 'D';
        make_card 7 'S';
        make_card 13 'C';
        make_card 2 'D';
        make_card 7 'C';
        make_card 7 'H';
      ]
      true;
    straight_test "no straight present"
      [
        make_card 1 'S';
        make_card 2 'C';
        make_card 10 'D';
        make_card 3 'D';
        make_card 7 'H';
        make_card 12 'C';
        make_card 11 'C';
      ]
      false;
    straight_test "[ 1; 2; 3; 4; 5 ] straight present"
      [
        make_card 1 'S';
        make_card 2 'C';
        make_card 4 'D';
        make_card 3 'D';
        make_card 7 'H';
        make_card 12 'C';
        make_card 5 'C';
      ]
      true;
    straight_test "[ 2; 3; 4; 5 ; 6] straight present"
      [
        make_card 6 'S';
        make_card 2 'C';
        make_card 4 'H';
        make_card 3 'D';
        make_card 10 'H';
        make_card 12 'C';
        make_card 5 'S';
      ]
      true;
    straight_test "[ 3; 4; 5 ; 6; 7] straight present"
      [
        make_card 6 'S';
        make_card 7 'C';
        make_card 4 'H';
        make_card 3 'D';
        make_card 1 'H';
        make_card 12 'C';
        make_card 5 'S';
      ]
      true;
    straight_test "[ 4; 5 ; 6; 7; 8] straight present"
      [
        make_card 6 'S';
        make_card 7 'C';
        make_card 4 'H';
        make_card 8 'D';
        make_card 1 'H';
        make_card 12 'C';
        make_card 5 'S';
      ]
      true;
    straight_test "[ 5 ; 6; 7; 8; 9] straight present"
      [
        make_card 6 'S';
        make_card 7 'C';
        make_card 9 'H';
        make_card 8 'D';
        make_card 1 'H';
        make_card 12 'C';
        make_card 5 'S';
      ]
      true;
    straight_test "[ 6; 7; 8; 9; 10] straight present"
      [
        make_card 6 'S';
        make_card 7 'C';
        make_card 9 'H';
        make_card 8 'D';
        make_card 1 'H';
        make_card 4 'C';
        make_card 10 'S';
      ]
      true;
    straight_test "[ 7; 8; 9; 10; 11] straight present"
      [
        make_card 11 'S';
        make_card 7 'C';
        make_card 9 'H';
        make_card 8 'D';
        make_card 2 'H';
        make_card 4 'C';
        make_card 10 'S';
      ]
      true;
    straight_test "[ 8; 9; 10; 11; 12] straight present"
      [
        make_card 11 'S';
        make_card 12 'C';
        make_card 9 'H';
        make_card 8 'D';
        make_card 2 'H';
        make_card 4 'C';
        make_card 10 'S';
      ]
      true;
    straight_test "[ 9; 10; 11; 12; 13] straight present"
      [
        make_card 11 'S';
        make_card 12 'C';
        make_card 9 'H';
        make_card 13 'D';
        make_card 2 'H';
        make_card 9 'C';
        make_card 10 'S';
      ]
      true;
    straight_test "[ 10; 11; 12; 13; 1] straight present"
      [
        make_card 11 'S';
        make_card 12 'C';
        make_card 1 'H';
        make_card 13 'D';
        make_card 2 'H';
        make_card 9 'C';
        make_card 10 'S';
      ]
      true;
    flush_test "no flush present"
      [
        make_card 1 'S';
        make_card 12 'C';
        make_card 7 'H';
        make_card 13 'D';
        make_card 2 'H';
        make_card 9 'C';
        make_card 10 'S';
      ]
      false;
    flush_test "flush of spades present"
      [
        make_card 1 'S';
        make_card 12 'C';
        make_card 7 'S';
        make_card 13 'D';
        make_card 2 'S';
        make_card 9 'S';
        make_card 10 'S';
      ]
      true;
    flush_test "flush of clubs present"
      [
        make_card 1 'C';
        make_card 12 'C';
        make_card 7 'C';
        make_card 13 'D';
        make_card 2 'C';
        make_card 9 'C';
        make_card 6 'H';
      ]
      true;
    flush_test "flush of hearts present"
      [
        make_card 10 'H';
        make_card 12 'H';
        make_card 7 'S';
        make_card 13 'H';
        make_card 2 'H';
        make_card 9 'C';
        make_card 6 'H';
      ]
      true;
    flush_test "flush of diamonds present"
      [
        make_card 10 'H';
        make_card 12 'D';
        make_card 7 'D';
        make_card 13 'D';
        make_card 2 'D';
        make_card 9 'C';
        make_card 6 'D';
      ]
      true;
    full_house_test "no full house present"
      [
        make_card 10 'H';
        make_card 12 'D';
        make_card 7 'S';
        make_card 13 'C';
        make_card 2 'D';
        make_card 9 'C';
        make_card 6 'H';
      ]
      false;
    full_house_test "full house present"
      [
        make_card 10 'H';
        make_card 10 'D';
        make_card 7 'S';
        make_card 13 'C';
        make_card 2 'D';
        make_card 7 'C';
        make_card 7 'H';
      ]
      true;
    full_house_test "two three of a kinds counts as a full house"
      [
        make_card 10 'H';
        make_card 10 'D';
        make_card 7 'S';
        make_card 10 'C';
        make_card 2 'D';
        make_card 7 'C';
        make_card 7 'H';
      ]
      true;
    four_of_a_kind_test "no four of a kind present"
      [
        make_card 10 'H';
        make_card 4 'D';
        make_card 10 'S';
        make_card 9 'C';
        make_card 2 'D';
        make_card 7 'C';
        make_card 4 'H';
      ]
      false;
    four_of_a_kind_test "four of a kind present"
      [
        make_card 10 'H';
        make_card 10 'D';
        make_card 10 'S';
        make_card 10 'C';
        make_card 2 'D';
        make_card 7 'C';
        make_card 4 'H';
      ]
      true;
    straight_flush_test "no straight flush present"
      [
        make_card 10 'H';
        make_card 2 'D';
        make_card 10 'S';
        make_card 7 'C';
        make_card 2 'D';
        make_card 7 'C';
        make_card 4 'H';
      ]
      false;
    straight_flush_test "straight flush present"
      [
        make_card 1 'H';
        make_card 2 'H';
        make_card 3 'H';
        make_card 4 'H';
        make_card 5 'H';
        make_card 11 'C';
        make_card 9 'S';
      ]
      true;
    straight_flush_test
      "there is a straight and a flush, but not relating to the same cards so \
       it isn't a straight flush"
      [
        make_card 1 'H';
        make_card 2 'H';
        make_card 3 'S';
        make_card 4 'H';
        make_card 5 'H';
        make_card 11 'C';
        make_card 9 'H';
      ]
      false;
    royal_flush_test "no royal flush present"
      [
        make_card 1 'H';
        make_card 6 'D';
        make_card 3 'S';
        make_card 12 'H';
        make_card 5 'D';
        make_card 11 'C';
        make_card 9 'H';
      ]
      false;
    royal_flush_test "royal flush present"
      [
        make_card 10 'H';
        make_card 11 'H';
        make_card 13 'H';
        make_card 12 'H';
        make_card 1 'H';
        make_card 11 'C';
        make_card 9 'S';
      ]
      true;
    royal_flush_test
      "flush and highest straight present, but not for same cards so it isn't \
       a royal flush"
      [
        make_card 10 'H';
        make_card 11 'D';
        make_card 13 'H';
        make_card 12 'H';
        make_card 1 'H';
        make_card 11 'C';
        make_card 9 'H';
      ]
      false;
  ]

let rec card_printer cl =
  match cl with
  | [] -> ""
  | h :: t -> str_card h ^ "; " ^ card_printer t

(** [get_rank_test name input] constructs an OUnit test named [name] that
    compares [expected_output] with [get_rank input] *)
let get_rank_test (name : string) (input : Card.t list)
    (expected_output : int * Card.t list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Hand.get_rank (Hand.make_hand input))
    ~cmp:(fun (i1, c1) (i2, c2) -> i1 = i2 && card_list_equal c1 c2)
    ~printer:(fun (i1, c1) ->
      "Rank: " ^ string_of_int i1 ^ ", cards: " ^ card_printer c1)

let get_rank_tests =
  [
    get_rank_test "testing high card"
      (create_card_list
         [
           (1, 'S'); (5, 'H'); (7, 'S'); (9, 'C'); (11, 'S'); (12, 'D'); (8, 'H');
         ])
      ( 1,
        create_card_list [ (1, 'S'); (9, 'C'); (11, 'S'); (12, 'D'); (8, 'H') ]
      );
    get_rank_test "testing high card"
      (create_card_list
         [
           (2, 'H');
           (4, 'S');
           (7, 'D');
           (10, 'C');
           (11, 'S');
           (13, 'H');
           (1, 'D');
         ])
      ( 1,
        create_card_list [ (7, 'D'); (10, 'C'); (11, 'S'); (13, 'H'); (1, 'D') ]
      );
    get_rank_test "testing high card"
      (create_card_list
         [
           (3, 'D'); (5, 'S'); (6, 'H'); (9, 'C'); (12, 'S'); (13, 'H'); (1, 'C');
         ])
      ( 1,
        create_card_list [ (6, 'H'); (9, 'C'); (12, 'S'); (13, 'H'); (1, 'C') ]
      );
    get_rank_test "testing high card"
      (create_card_list
         [
           (2, 'C'); (3, 'H'); (5, 'D'); (7, 'S'); (8, 'H'); (10, 'D'); (1, 'S');
         ])
      (1, create_card_list [ (5, 'D'); (7, 'S'); (8, 'H'); (10, 'D'); (1, 'S') ]);
    get_rank_test "testing high card"
      (create_card_list
         [
           (1, 'H');
           (3, 'S');
           (6, 'D');
           (8, 'C');
           (11, 'H');
           (12, 'S');
           (13, 'D');
         ])
      ( 1,
        create_card_list [ (1, 'H'); (8, 'C'); (11, 'H'); (12, 'S'); (13, 'D') ]
      );
    get_rank_test "testing pair"
      (create_card_list
         [
           (1, 'S'); (1, 'H'); (2, 'H'); (3, 'C'); (7, 'S'); (8, 'C'); (11, 'H');
         ])
      (2, create_card_list [ (1, 'S'); (1, 'H'); (7, 'S'); (8, 'C'); (11, 'H') ]);
    get_rank_test "testing pair"
      (create_card_list
         [
           (1, 'S'); (1, 'D'); (2, 'S'); (10, 'C'); (7, 'H'); (12, 'C'); (5, 'C');
         ])
      ( 2,
        create_card_list [ (1, 'S'); (1, 'D'); (7, 'H'); (10, 'C'); (12, 'C') ]
      );
    get_rank_test "testing pair"
      (create_card_list
         [
           (4, 'S'); (7, 'D'); (2, 'S'); (2, 'D'); (3, 'H'); (12, 'C'); (5, 'C');
         ])
      (2, create_card_list [ (2, 'S'); (2, 'D'); (7, 'D'); (12, 'C'); (5, 'C') ]);
    get_rank_test "testing pair"
      (create_card_list
         [
           (6, 'S');
           (6, 'D');
           (8, 'S');
           (10, 'C');
           (11, 'H');
           (12, 'C');
           (13, 'S');
         ])
      ( 2,
        create_card_list [ (6, 'S'); (6, 'D'); (11, 'H'); (12, 'C'); (13, 'S') ]
      );
    get_rank_test "testing pair"
      (create_card_list
         [
           (9, 'H'); (9, 'S'); (4, 'D'); (7, 'C'); (12, 'S'); (1, 'H'); (2, 'D');
         ])
      (2, create_card_list [ (9, 'H'); (9, 'S'); (12, 'S'); (1, 'H'); (7, 'C') ]);
    get_rank_test "testing three of a kind"
      (create_card_list
         [
           (1, 'S'); (1, 'H'); (1, 'D'); (7, 'S'); (3, 'C'); (8, 'C'); (11, 'H');
         ])
      (4, create_card_list [ (1, 'S'); (1, 'H'); (1, 'D'); (8, 'C'); (11, 'H') ]);
    get_rank_test "testing three of a kind"
      (create_card_list
         [
           (4, 'H'); (4, 'S'); (4, 'D'); (6, 'S'); (9, 'C'); (11, 'D'); (13, 'H');
         ])
      ( 4,
        create_card_list [ (4, 'H'); (4, 'S'); (4, 'D'); (11, 'D'); (13, 'H') ]
      );
    get_rank_test "testing three of a kind"
      (create_card_list
         [
           (4, 'H'); (4, 'S'); (4, 'D'); (6, 'C'); (9, 'S'); (11, 'H'); (13, 'D');
         ])
      ( 4,
        create_card_list [ (4, 'H'); (4, 'S'); (4, 'D'); (11, 'H'); (13, 'D') ]
      );
    get_rank_test "testing three of a kind"
      (create_card_list
         [
           (12, 'H');
           (12, 'S');
           (3, 'C');
           (5, 'S');
           (6, 'H');
           (8, 'D');
           (12, 'D');
         ])
      ( 4,
        create_card_list [ (12, 'S'); (12, 'D'); (6, 'H'); (8, 'D'); (12, 'H') ]
      );
    get_rank_test "testing two pairs"
      (create_card_list
         [
           (1, 'S'); (1, 'H'); (3, 'C'); (8, 'C'); (11, 'H'); (2, 'D'); (2, 'S');
         ])
      (3, create_card_list [ (1, 'S'); (1, 'H'); (2, 'D'); (2, 'S'); (11, 'H') ]);
    get_rank_test "testing two pairs"
      (create_card_list
         [
           (3, 'H');
           (3, 'S');
           (12, 'S');
           (1, 'H');
           (2, 'D');
           (10, 'D');
           (10, 'C');
         ])
      ( 3,
        create_card_list [ (3, 'H'); (3, 'S'); (10, 'D'); (10, 'C'); (1, 'H') ]
      );
    get_rank_test "testing two pairs"
      (create_card_list
         [
           (2, 'H'); (2, 'S'); (7, 'D'); (7, 'C'); (11, 'S'); (13, 'H'); (1, 'D');
         ])
      (3, create_card_list [ (2, 'H'); (2, 'S'); (7, 'D'); (7, 'C'); (1, 'D') ]);
    get_rank_test "testing straight"
      (create_card_list
         [
           (1, 'S'); (2, 'H'); (3, 'D'); (4, 'S'); (5, 'C'); (8, 'C'); (11, 'H');
         ])
      (5, create_card_list [ (1, 'S'); (2, 'H'); (3, 'D'); (4, 'S'); (5, 'C') ]);
    get_rank_test "testing straight"
      (create_card_list
         [
           (10, 'S');
           (11, 'D');
           (12, 'S');
           (13, 'C');
           (1, 'H');
           (2, 'C');
           (3, 'S');
         ])
      ( 5,
        create_card_list
          [ (10, 'S'); (11, 'D'); (12, 'S'); (13, 'C'); (1, 'H') ] );
    get_rank_test "testing straight"
      (create_card_list
         [
           (5, 'S'); (6, 'D'); (7, 'S'); (8, 'C'); (9, 'H'); (10, 'C'); (2, 'S');
         ])
      (5, create_card_list [ (6, 'D'); (7, 'S'); (8, 'C'); (9, 'H'); (10, 'C') ]);
    get_rank_test "testing straight"
      (create_card_list
         [
           (9, 'H');
           (10, 'S');
           (11, 'D');
           (12, 'C');
           (13, 'S');
           (1, 'D');
           (2, 'C');
         ])
      ( 5,
        create_card_list
          [ (10, 'S'); (11, 'D'); (12, 'C'); (13, 'S'); (1, 'D') ] );
    get_rank_test "testing flush"
      (create_card_list
         [
           (1, 'S');
           (5, 'S');
           (7, 'S');
           (8, 'S');
           (10, 'S');
           (11, 'C');
           (11, 'H');
         ])
      (6, create_card_list [ (1, 'S'); (5, 'S'); (7, 'S'); (8, 'S'); (10, 'S') ]);
    get_rank_test "testing flush"
      (create_card_list
         [
           (1, 'S');
           (5, 'S');
           (7, 'S');
           (8, 'S');
           (11, 'C');
           (11, 'H');
           (10, 'S');
         ])
      (6, create_card_list [ (1, 'S'); (5, 'S'); (7, 'S'); (8, 'S'); (10, 'S') ]);
    get_rank_test "testing flush"
      (create_card_list
         [
           (2, 'H'); (6, 'H'); (9, 'H'); (7, 'S'); (10, 'H'); (13, 'H'); (8, 'D');
         ])
      ( 6,
        create_card_list [ (2, 'H'); (6, 'H'); (9, 'H'); (10, 'H'); (13, 'H') ]
      );
    get_rank_test "testing flush"
      (create_card_list
         [
           (2, 'D');
           (4, 'D');
           (6, 'D');
           (9, 'D');
           (13, 'D');
           (10, 'S');
           (11, 'H');
         ])
      (6, create_card_list [ (2, 'D'); (4, 'D'); (6, 'D'); (9, 'D'); (13, 'D') ]);
    get_rank_test "testing flush"
      (create_card_list
         [
           (3, 'C'); (5, 'C'); (7, 'C'); (9, 'C'); (11, 'C'); (13, 'S'); (1, 'H');
         ])
      (6, create_card_list [ (3, 'C'); (5, 'C'); (7, 'C'); (9, 'C'); (11, 'C') ]);
    get_rank_test "testing full house"
      (create_card_list
         [
           (1, 'S'); (1, 'H'); (2, 'S'); (11, 'C'); (11, 'H'); (2, 'C'); (2, 'D');
         ])
      (7, create_card_list [ (2, 'C'); (2, 'D'); (2, 'S'); (1, 'S'); (1, 'H') ]);
    get_rank_test "testing full house"
      (create_card_list
         [
           (3, 'S'); (3, 'D'); (3, 'C'); (7, 'H'); (7, 'D'); (7, 'S'); (12, 'C');
         ])
      (7, create_card_list [ (7, 'H'); (7, 'D'); (7, 'S'); (3, 'S'); (3, 'D') ]);
    get_rank_test "testing full house"
      (create_card_list
         [
           (5, 'H'); (5, 'S'); (5, 'D'); (9, 'C'); (9, 'S'); (10, 'H'); (1, 'D');
         ])
      (7, create_card_list [ (5, 'H'); (5, 'S'); (5, 'D'); (9, 'C'); (9, 'S') ]);
    get_rank_test "testing full house"
      (create_card_list
         [
           (7, 'D');
           (7, 'S');
           (7, 'C');
           (11, 'H');
           (11, 'S');
           (12, 'C');
           (2, 'H');
         ])
      ( 7,
        create_card_list [ (7, 'D'); (7, 'S'); (7, 'C'); (11, 'H'); (11, 'S') ]
      );
    get_rank_test "testing four of a kind"
      (create_card_list
         [
           (1, 'S'); (1, 'H'); (1, 'C'); (2, 'S'); (11, 'C'); (1, 'D'); (10, 'H');
         ])
      (8, create_card_list [ (1, 'S'); (1, 'H'); (1, 'C'); (1, 'D'); (11, 'C') ]);
    get_rank_test "testing four of a kind"
      (create_card_list
         [
           (8, 'S'); (8, 'H'); (8, 'D'); (8, 'C'); (5, 'H'); (6, 'S'); (7, 'D');
         ])
      (8, create_card_list [ (8, 'S'); (8, 'H'); (8, 'D'); (8, 'C'); (7, 'D') ]);
    get_rank_test "testing four of a kind"
      (create_card_list
         [
           (10, 'H');
           (10, 'S');
           (3, 'S');
           (5, 'H');
           (7, 'D');
           (10, 'D');
           (10, 'C');
         ])
      ( 8,
        create_card_list
          [ (10, 'H'); (10, 'S'); (10, 'D'); (10, 'C'); (7, 'D') ] );
    get_rank_test "testing straight flush"
      (create_card_list
         [
           (1, 'S'); (2, 'S'); (3, 'S'); (4, 'S'); (5, 'S'); (11, 'C'); (11, 'H');
         ])
      (9, create_card_list [ (1, 'S'); (2, 'S'); (3, 'S'); (4, 'S'); (5, 'S') ]);
    get_rank_test "testing straight flush"
      (create_card_list
         [
           (9, 'H');
           (10, 'H');
           (11, 'H');
           (12, 'H');
           (13, 'H');
           (2, 'C');
           (3, 'S');
         ])
      ( 9,
        create_card_list
          [ (9, 'H'); (10, 'H'); (11, 'H'); (12, 'H'); (13, 'H') ] );
    get_rank_test "testing straight flush"
      (create_card_list
         [
           (6, 'C');
           (7, 'C');
           (8, 'C');
           (9, 'C');
           (10, 'C');
           (11, 'S');
           (13, 'H');
         ])
      (9, create_card_list [ (6, 'C'); (7, 'C'); (8, 'C'); (9, 'C'); (10, 'C') ]);
    get_rank_test "testing straight flush"
      (create_card_list
         [
           (3, 'S'); (4, 'S'); (5, 'S'); (6, 'S'); (7, 'S'); (10, 'C'); (1, 'H');
         ])
      (9, create_card_list [ (3, 'S'); (4, 'S'); (5, 'S'); (6, 'S'); (7, 'S') ]);
    get_rank_test "testing royal flush"
      (create_card_list
         [
           (10, 'H');
           (11, 'H');
           (12, 'H');
           (13, 'H');
           (1, 'H');
           (11, 'C');
           (9, 'H');
         ])
      ( 10,
        create_card_list
          [ (10, 'H'); (11, 'H'); (12, 'H'); (13, 'H'); (1, 'H') ] );
    get_rank_test "testing royal flush"
      (create_card_list
         [
           (10, 'H');
           (11, 'H');
           (12, 'H');
           (13, 'H');
           (1, 'H');
           (5, 'S');
           (6, 'C');
         ])
      ( 10,
        create_card_list
          [ (10, 'H'); (11, 'H'); (12, 'H'); (13, 'H'); (1, 'H') ] );
    get_rank_test "testing royal flush"
      (create_card_list
         [
           (10, 'D');
           (11, 'D');
           (12, 'D');
           (13, 'D');
           (2, 'S');
           (4, 'C');
           (1, 'D');
         ])
      ( 10,
        create_card_list
          [ (10, 'D'); (11, 'D'); (12, 'D'); (13, 'D'); (1, 'D') ] );
  ]

(* not sure how to test randomly generated cards *)
let player_test (name : string) (expected : int) (deck : Deck.t) (amt : int) =
  name >:: fun _ ->
  assert_equal expected
    (Player.get_balance (Player.make_player deck amt "test"))

(** [player_move_test name p1_owes p1_balance p2_balance com p1 p2] constructs
    an OUnit test named [name] that asserts the value of p1.owes, p2.owes, and
    p1.balance after calling [Player.make_move com p1 p2] *)
let player_move_test (name : string) (p1_owes : int) (p1_balance : int)
    (p2_owes : int) (com : Command.command) (p1 : Player.t) (p2 : Player.t) =
  name >:: fun _ ->
  let _ = Player.make_move com p1 p2 in
  assert_equal
    (p1_owes, p1_balance, p2_owes)
    (Player.get_owes p1, Player.get_balance p1, Player.get_owes p2)
    ~printer:(fun (a, b, c) ->
      "player 1 owes: " ^ string_of_int a ^ "\nplayer 1 balance "
      ^ string_of_int b ^ "\nplayer 2 owes " ^ string_of_int c)

(** Need to initialize many times since fields are being mutated and OUnit tests
    don't guarantee order*)
let p1_test1 = Player.make_player deck1 250 "player1"

let p1_test2 = Player.make_player deck1 250 "player2"
let p1_test3 = Player.make_player deck1 250 "player1"
let p1_test4 = Player.make_player deck1 50 "player1"
let p1_test5 = Player.make_player deck1 50 "player1"
let p1_test6 = Player.make_player deck1 250 "player1"
let p1_test7 = Player.make_player deck1 250 "player1"
let p1_test8 = Player.make_player deck1 250 "player1"
let p2_test1 = Player.make_player deck1 250 "player2"
let p2_test2 = Player.make_player deck1 250 "player2"
let p2_test3 = Player.make_player deck1 250 "player2"
let p2_test4 = Player.make_player deck1 250 "player2"
let p2_test5 = Player.make_player deck1 250 "player2"
let p2_test6 = Player.make_player deck1 50 "player2"
let p2_test7 = Player.make_player deck1 50 "player2"
let p2_test8 = Player.make_player deck1 250 "player1"

let player_tests =
  print_endline "being exectued now";
  [
    player_test "testing make" 0 deck1 0;
    player_move_test "p1 plays check without owing anything" 0 250 0 Check
      p1_test1 p2_test1;
    player_move_test "p1 plays fold " 0 250 0 Fold p1_test1 p2_test1;
    player_move_test "p1 plays raise 10 without owing anything " 0 240 10
      (Raise 10) p1_test1 p2_test1;
    (let _ = Player.make_move (Raise 50) p2_test3 p1_test3 in
     player_move_test "p1 goes all in while owing 50 " 0 0 200 AllIn p1_test3
       p2_test3);
    ( "p1 raises 250 when they have 240" >:: fun _ ->
      assert_raises
        (Player.RaiseFailed (240, 0, 250))
        (fun () ->
          Player.make_move (Raise 10) p1_test2 p2_test2;
          Player.make_move (Raise 250) p1_test2 p2_test2) );
    ( "p1 raises calls when they don't have enough money" >:: fun _ ->
      assert_raises (Player.CallFailed 80) (fun () ->
          Player.make_move (Raise 80) p2_test4 p1_test4;
          Player.make_move Call p1_test4 p2_test4) );
    (let _ = Player.make_move (Raise 80) p2_test5 p1_test5 in
     player_move_test "p1 goes all in when they don't have enough money" 0 0 0
       AllIn p1_test5 p2_test5);
    (let _ = Player.make_move AllIn p1_test6 p2_test6 in
     player_move_test "p2 goes all in while owing more than their balance " 0 0
       0 AllIn p2_test6 p1_test6);
    ( "p1 raises calls when they don't have enough money" >:: fun _ ->
      assert_raises Player.AllInFailed (fun () ->
          Player.make_move AllIn p1_test7 p2_test7;
          Player.make_move AllIn p1_test7 p2_test7) );
    (let _ = Player.make_move (Raise 50) p2_test8 p1_test8 in
     player_move_test "p1 Calls while owing 50 " 0 200 0 Call p1_test8 p2_test8);
  ]

let compare_test (name : string) (expected : Player.t) (p1 : Player.t)
    (p2 : Player.t) (rank : int) (h1 : Card.t list) (h2 : Card.t list) =
  name >:: fun _ -> assert_equal expected (Compare.compare_hand p1 p2 rank h1 h2)

(* placeholder players for now but have no correlation to the hands being fed
   into the test cases *)
let p1 = Player.make_player deck1 0 "p1"
let p2 = Player.make_player deck1 0 "p2"
let king = Card.make_card 13 'H'
let queen = Card.make_card 12 'H'
let jack = Card.make_card 11 'H'
let ten = Card.make_card 10 'H'
let nine = Card.make_card 9 'H'
let eight = Card.make_card 8 'H'
let seven = Card.make_card 7 'H'
let six = Card.make_card 6 'H'
let five = Card.make_card 5 'H'
let four = Card.make_card 4 'H'
let three = Card.make_card 3 'H'
let two = Card.make_card 2 'H'
let ace = Card.make_card 1 'H'
let royal_flush = [ king; queen; jack; ten; ace ]
let straight_flush1 = [ eight; queen; jack; ten; nine ]
let straight_flush2 = [ eight; seven; six; five; four ]
let full_house1 = [ four; four; four; two; two ]
let full_house2 = [ ten; ten; ten; king; king ]
let full_house3 = [ four; four; four; ten; ten ]
let flush1 = [ eight; ten; six; queen; two ]
let flush2 = [ seven; six; nine; five; four ]
let straight1 = [ ten; nine; seven; eight; six ]
let straight2 = [ four; eight; seven; six; five ]
let three_of_a_kind1 = [ four; four; four; ten; two ]
let three_of_a_kind2 = [ ten; ten; ten; queen; king ]
let three_of_a_kind3 = [ four; four; four; ten; eight ]

(* ace should be highest card, but its sorted as 1 *)
let compare_tests =
  [
    compare_test "straight flush" p1 p1 p2 2 straight_flush1 straight_flush2;
    compare_test "full house same three" p1 p1 p2 4 full_house3 full_house1;
    compare_test "full house diff three" p1 p1 p2 4 full_house2 full_house3;
    compare_test "flush" p1 p1 p2 5 flush1 flush2;
    compare_test "straight" p1 p1 p2 6 straight1 straight2;
    compare_test "three of a kind same three" p1 p1 p2 7 three_of_a_kind3
      three_of_a_kind1;
    compare_test "three of a kind diff three" p1 p1 p2 7 three_of_a_kind2
      three_of_a_kind1;
  ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           card_tests;
           deck_tests;
           hole_tests;
           hand_tests;
           parse_tests;
           player_tests;
           get_rank_tests;
           compare_tests;
         ]

let _ = run_test_tt_main suite
