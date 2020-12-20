(** 
   Representation of pokemon data.

   This module represents the data of a pokemon file, including their name, 
   type, level, base HP, HP, attack, defense, experience, move set, and 
   caught status. It also handles loading of a pokemon's data from JSON.
*)

(** Raised when an invalid pokemon is encountered. *)
exception InvalidPokemon of string

(** Raised when an invalid pokemon type is encountered. *)
exception InvalidPokemonType of string 

(** The type of pokemon move types. *)
type poke_type = 
  | Bug | Dark | Dragon | Electric | Fairy | Fighting | Fire | Flying | Ghost 
  | Grass | Ground | Ice | Normal | Poison | Psychic | Rock | Steel | Water 

(** The type of pokemon move names. *)
type move_name = string

(** The type of values representing a pokemon's move. *)
type move = {
  move_type: poke_type;
  move_name: move_name;
}

(** The type of values representing a pokemon's stats. *)
type stats = {
  level: int;
  base_hp: int;
  hp: int;
  attack: int;
  defense: int;
  curr_exp: int;
  level_up_exp: int;
}

(** The type of values representing a pokemon. *)
type pokemon = {
  name: string;
  poke_type: poke_type;
  stats: stats;
  caught: bool;
  move_set: move list
}

(** [poke_list_from_json json] is the a list of pokemon from the JSON [json].
    Requires: [j] is a valid JSON for a list of pokemon. *)
val poke_list_from_json : Yojson.Basic.t -> pokemon list

(** [type_from_string type] is the string representation of poke_type [type].
    Requires: [type] is capitalized.
    Raises: InvalidPokemonType if [type] is not a valid pokemon type. *)
val type_from_string : string -> poke_type

val string_from_type : poke_type -> string

val attack_effectiveness : pokemon -> pokemon -> move -> string

(** [opponent_move p] is the move the opponenet pokemon [p] chooses. *)
val opponent_move : pokemon -> move 

(** [battle_damage p1 p2 move] is a pokemon [p1] after damage is inflicted
    by a move [move] of pokemon [p2]. *)
val battle_damage : pokemon -> pokemon -> move -> pokemon

(** [level_up p] is pokemon [p] with [p]'s level incremented by one if 
    [p]'s curr_exp exceeds [p]'s level_up_exp and [p]'s hp, attack, and defense
    are incremented accordingly. *)
val level_up : pokemon -> pokemon

(** [increase_exp p1 p2] is pokemon [p1]'s curr_exp incremented based on
    the stats of pokemon [p2]. *)
val increase_exp : pokemon -> pokemon -> pokemon