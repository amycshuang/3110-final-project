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

let stats_of_json j = {
  level = j |> member "level" |> to_int;
  hp = j |> member "hp" |> to_int;
  attack = j |> member "attack" |> to_int;
  defense = j |> member "defense" |> to_int;
  curr_exp = j |> member "curr_exp" |> to_int;
  level_up_exp = j |> member "level_up_exp" |> to_int; 
}

let type_from_string = function 
  | "Bug" -> Bug
  | "Dark" -> Dark
  | "Dragon" -> Dragon
  | "Electric" -> Electric
  | "Fighting" -> Fighting
  | "Fire" -> Fire
  | "Flying" -> Flying
  | "Ghost" -> Ghost
  | "Grass" -> Grass
  | "Ice" -> Ice
  | "Normal" -> Normal
  | "Poison" -> Poison
  | "Psychic" -> Psychic
  | "Rock" -> Rock
  | "Steel" -> Steel
  | "Water" -> Water
  | _ -> raise (InvalidPokemon ("this pokemon does not have a type"))

let moves_of_json j = {
  move_type = j |> member "move_type" |> to_string |> type_from_string;
  move_name = j |> member "move_name" |> to_string; 
}

let poke_from_json j = {
  name = j |> member "name" |> to_string;
  poke_type = j |> member "poke_type" |> to_string |> type_from_string;
  stats = j |> member "stats" |> stats_of_json;
  caught = j |> member "caught" |> to_bool;
  move_set = j |> member "move_set" |> to_list |> List.map moves_of_json;
} 

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
    } in { pokemon with stats = new_stats;}
  else pokemon

let increase_exp p1 p2 = 
  let p2_lvl = float_of_int p2.stats.level in 
  let exp = int_of_float (p2_lvl *. 0.5) in 
  let curr_stats = p1.stats in 
  let new_stats = 
    { curr_stats with curr_exp = curr_stats.level_up_exp + exp; } in 
  { p1 with stats = new_stats;} 