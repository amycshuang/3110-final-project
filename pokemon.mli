(** 
   Representation of pokemon data.

   This module represents the data of a pokemon file, including their name, 
   type, level, HP, attack, defense, experience, move set, and caught status. 
   It also handles loading of a pokemon's data from JSON.
*)

(** The abstract type of values representing a pokemon. *)
type t

(** The name of a pokemon *)
type name = string

(** The type of a pokemon's type *)
type poke_type

(** The type of a pokemon's level *)
type level = int

(** The type of a pokemon's HP stat *)
type hp = int

(** The type of a pokemon's attack stat *)
type attack = int

(** The type of a pokemon's defense stat *)
type defense = int

(** The type of a pokemon's current experience *)
type curr_exp = int

(** The type of the experience needed for the pokemon to level up *)
type level_up_exp = int

(** The type of a pokemon's current caught status *) 
type caught = bool

(** The type of a pokemon's move *)
type move 

(** The type of a pokemon's move name *)
type move_name = string

(** Raised when an invalid pokemon is encountered. *)
exception InvalidPokemon of string

(** [poke_from_json j] is the pokemon that [j] represents.
    Requires: [j] is a valid JSON pokemon representation. *)
val poke_from_json : Yojson.Basic.t -> t

(** [get_name t] is the name of pokemon [t]. *)
val get_name : t -> name

(** [get_poke_type t] is the poke_type of pokemon [t]. *)
val get_poke_type : t -> poke_type

(** [get_poke_type t] is the level of pokemon [t]. *)
val get_level : t -> level

(** [get_hp t] is the hp of pokemon [t]. *)
val get_hp : t -> hp

(** [get_attack t] is the attack of pokemon [t]. *)
val get_attack : t -> attack

(** [get_defense t] is the defense of pokemon [t]. *)
val get_defense : t -> defense

(** [get_curr_exp t is the current experience of pokemon [t]. *)
val get_curr_exp: t -> curr_exp

(** [get_level_up_exp t] is the level_up_exp of pokemon [t]. *)
val get_level_up_exp : t -> level_up_exp

(** [get_caught t] is the caught status of pokemon [t]. *)
val get_caught : t -> caught

(** [get_move t move_name] is the [move] represented by [move_name]. *)
val get_move : t -> move_name -> move

(** [valid_move_name pkm move_name] is true if move_name is a valid move name
    for one of [pokemon]'s moves  *)
val valid_move_name : t -> string -> bool

(** [type_from_string type] is the string representation of poke_type [type] *)
val type_from_string : string -> poke_type

(** [level_up t] is pokemon [t] with [t]'s level incremented by one if 
    [t]'s curr_exp exceeds [t]'s level_up_exp and [t]'s hp, attack, and defense
    are incremented accordinglt. *)
val level_up : t -> t

(** [increase_exp p1 p2] is pokemon [p1]'s curr_exp incremented based on
    the stats of pokemon [p2]. *)
val increase_exp : t -> t -> t