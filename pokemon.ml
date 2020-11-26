open Yojson.Basic.Util

type poke_type = 
  | Bug | Dark | Dragon | Electric | Fighting | Fire | Flying | Ghost 
  | Grass | Ground | Ice | Normal | Poison | Psychic | Rock | Steel | Water 

exception InvalidPokemon of string
exception InvalidPokemonType of string 

type move_name = string
type move = {
  move_type: poke_type;
  move_name: move_name;
}

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

(** [stats_of_json j] is the pokemon stat represented by [j]. *)
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
  | "Ground" -> Ground
  | "Ice" -> Ice
  | "Normal" -> Normal
  | "Poison" -> Poison
  | "Psychic" -> Psychic
  | "Rock" -> Rock
  | "Steel" -> Steel
  | "Water" -> Water
  | _ -> raise (InvalidPokemonType ("this pokemon type is not valid"))

(** [moves_of_json j] is the pokemon move represented by [j] *)
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

let poke_list_from_json j = j |> to_list |> List.map poke_from_json

let opponent_move pokemon = 
  List.nth pokemon.move_set (Random.int (List.length pokemon.move_set)) 

let damage_multiplier pkm opp_pkm_mv = failwith "TODO"

let battle_damage pokemon move = failwith "TODO"

let level_up pokemon = 
  let curr_stats = pokemon.stats in
  if curr_stats.curr_exp >= curr_stats.level_up_exp then 
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
    { curr_stats with curr_exp = curr_stats.curr_exp + exp; } in 
  { p1 with stats = new_stats} 