(** 
   Representation of pokemon data.

   This module represents the data of a pokemon file, including their name, 
   type, level, HP, attack, defense, experience, move set, and caught status. 
   It also handles loading of a pokemon's data from JSON.
*)

type poke_type = 
  | Bug | Dark | Dragon | Electric | Fighting | Fire | Flying | Ghost 
  | Grass | Ground | Ice | Normal | Poison | Psychic | Rock | Steel | Water

type move = {
  move_type: poke_type;
  move_name: string;
}

(** The abstract type of values representing a pokemon. *)
type stats = {
  level: int;
  hp: int;
  attack: int;
  defense: int;
  curr_exp: int;
  level_up_exp: int;
}

type pokemon = {
  name: string;
  poke_type: poke_type;
  stats: stats;
  caught: bool;
  move_set: move list
}

(** Raised when an invalid pokemon is encountered. *)
exception InvalidPokemon of string

(** [poke_from_json j] is the pokemon that [j] represents.
    Requires: [j] is a valid JSON pokemon representation. *)
val poke_from_json : Yojson.Basic.t -> pokemon

(** [poke_list_from_json json] is the a list of pokemon from the JSON [json]. *)
val poke_list_from_json : Yojson.Basic.t -> pokemon list

(** [valid_move_name pkm move_name] is true if move_name is a valid move name
    for one of [pokemon]'s moves  *)
val valid_move_name : pokemon -> string -> bool

(** [type_from_string type] is the string representation of poke_type [type] *)
val type_from_string : string -> poke_type

(** [level_up t] is pokemon [t] with [t]'s level incremented by one if 
    [t]'s curr_exp exceeds [t]'s level_up_exp and [t]'s hp, attack, and defense
    are incremented accordinglt. *)
val level_up : pokemon -> pokemon

(** [increase_exp p1 p2] is pokemon [p1]'s curr_exp incremented based on
    the stats of pokemon [p2]. *)
val increase_exp : pokemon -> pokemon -> pokemon