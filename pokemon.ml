open Yojson.Basic.Util

type name = string
type poke_type = 
  | Bug | Dark | Dragon | Electric | Fighting | Fire | Flying | Ghost 
  | Grass | Ice | Normal | Poison | Psychic | Rock | Steel | Water
type level = int
type hp = int
type attack = int
type defense = int
type curr_exp = int
type level_up_exp = int
type caught = bool
exception InvalidPokemon of string

type move = {
  move_type: poke_type;
  move_name: string;
}

type stats = {
  level: level;
  hp: hp;
  attack: attack;
  defense: defense;
  curr_exp: curr_exp;
  level_up_exp: level_up_exp;
}

type t = {
  name: name;
  poke_type: poke_type;
  stats: stats;
  caught: caught;
  move_set: move list
}

let poke_from_json pokemon = 
  failwith "Unimplemented"

let get_name pokemon = pokemon.name

let get_poke_type pokemon = pokemon.poke_type 

let get_level pokemon = pokemon.stats.level

let get_hp pokemon = pokemon.stats.hp

let get_attack pokemon = pokemon.stats.attack

let get_defense pokemon = pokemon.stats.defense

let get_curr_exp pokemon = pokemon.stats.curr_exp

let get_level_up_exp pokemon = pokemon.stats.level_up_exp

let get_caught pokemon = pokemon.caught

let get_move pokemon move_name = 
  match List.filter (fun move' -> move'.move_name = move_name) 
          pokemon.move_set with
  | [] -> raise (InvalidPokemon "pokemon is not valid")
  | h :: t -> h

let level_up pokemon = 
  let curr_stats = pokemon.stats in
  if curr_stats.curr_exp > curr_stats.level_up_exp then 
    let new_stats = {
      level = curr_stats.level + 1;
      hp = curr_stats.hp + 2;
      attack = curr_stats.attack + 2;
      defense = curr_stats.defense + 2;
      curr_exp = curr_stats.level_up_exp - curr_stats.curr_exp;
      level_up_exp = curr_stats.level_up_exp + 2;
    } in 
    {
      name = pokemon.name;
      poke_type = pokemon.poke_type;
      stats = new_stats;
      caught = pokemon.caught;
      move_set = pokemon.move_set;
    }
  else pokemon

let increase_exp p1 p2 = 
  let p2_lvl = float_of_int p2.stats.level in 
  let exp = int_of_float (p2_lvl *. 0.5) in 
  let curr_stats = p1.stats in 
  let new_stats = {
    level = curr_stats.level;
    hp = curr_stats.hp;
    attack = curr_stats.attack;
    defense = curr_stats.defense;
    curr_exp = curr_stats.level_up_exp + exp;
    level_up_exp = curr_stats.level_up_exp;
  } in 
  { 
    name = p1.name;
    poke_type = p1.poke_type;
    stats = new_stats;
    caught = p1.caught;
    move_set = p1.move_set;
  }