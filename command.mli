(** 
   Representation of possible player commands for the chess game.
*)

(** The type [phrase] is a  string list that represents the phrase of a player
    command, ignoring whitespace. Each element of the list represents a word in
    the command. The list is in the same order as the words of the original
    player command. [phrase] cannot be the empty list.
    For example: 
    - ["attack with flamethrower"] is the phrase 
      [["attack"; "with"; "flamethrower"]]. *)
type phrase = string list

(** The type [command] is a player command that is decomposed
    into a verb and possibly a phrase. *)
type command = 
  | Quit
  | Yes
  | No
  | Run 
  | Catch
  | Bag 
  | Attack of phrase
  | Battle of phrase

(** Raised when an invalid attack is parsed. *)
exception InvalidAttack of string

(** Raised when an invalid pokemon to catch is parsed. *)
exception InvalidCatch of string

(** Raised when an invalid command is parsed. *)
exception InvalidCommand of string

(** [parse cmd] parses a player's input into a command [cmd]. The first
    word becomes the verb, and the rest of the words become the lowercased
    phrase. [parse cmd] is not case-sensitive.

    For example: 
    - [parse "attack with flamethrower"] is [Attack ["with"; "flamethrower"]]
    - [parse "    battle trainer   "] is [Battle ["trainer"]]
    - [parse "quit"] is [Quit]. 

    Requires: 
    - [cmd] contains only alphanumeric (A-Z, a-z, 0-9) and space 
      characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: 
    - [InvalidAttack atk] if [atk] is not a valid attack.
    - [InvalidCatch pkm] if it is invalid to catch the pokemon [pkm].
    - [InvalidCommand cmd] if [cmd] does not start with "quit", "run", "attack",
      "catch", or "battle" or if the command has unncessary words or more
      words than necessary. *)
val parse : Pokemon.t option -> string -> command
