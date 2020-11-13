(** 
   Representation of possible player commands for the chess game.
*)

(** The type [phrase] is a  string list that represents the phrase of a player
    command, ignoring whitespace. Each element of the list represents a word in
    the command. The list is in the same order as the words of the original
    player command. [phrase] cannot be the empty list. *)
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
  | Map of string
  | SPokemon of string
  | Attack of phrase
  | Battle of phrase

(** Raised when an invalid region is parsed. *)
exception InvalidRegion

(** Raised when an invalid command is parsed. *)
exception InvalidCommand of string

(** [parse_region region] is the map command associated with [region]. *)
val parse_region : phrase -> command

(** [parse_starter pkm] is the pokemon command associated with [pkm]. *)
val parse_starter: phrase -> command

(** [parse_yn cmd] is [Yes] if "yes" is parsed and is [No] if "no" is parsed. *)
val parse_yn : phrase -> command 

(** [parse cmd] parses a player's input into a command [cmd].*)
val parse : string -> (phrase -> command) -> command
