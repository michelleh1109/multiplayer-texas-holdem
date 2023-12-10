(** Representation of a user command.

    A command can be a regular command, a difficulty command used to set the
    difficulty level of the game, or a start command used to initiate a game.

    This module is also responsible for parsing commands from user input. *)

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
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes
    the verb. The rest, if applicable, become the argument. For example, a user
    can raise by typing raise 5, which would output [Raise 5]. Other commands
    don't include any arguments. For example, if a player types check, the
    funciton outputs [Check]. If the first verb of the user input is a valid
    command but the rest isn't, then the first command is accepted. For example:
    "fold afadfasfa" ouputs [Fold] *)

val parse_diff : string -> diff_command
(** [parse_diff str] parses a player's input into a [command_dif], as follows.
    The first word (i.e., consecutive sequence of non-space characters) of [str]
    becomes the verb. The rest, if applicable, become the argument.*)

val remove_spaces : string list -> string list
(** [remove_spaces lst] returns lst without the string "" inside it*)

val parse_start : string -> start_command
(** [parse_start str] parses a player's input into a [command_start], as
    follows. The first word (i.e., consecutive sequence of non-space characters)
    of [str] becomes the verb. The rest, if applicable, become the argument.*)
