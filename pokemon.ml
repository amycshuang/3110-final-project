open Yojson.Basic.Util

exception InvalidPokemon of string
exception InvalidPokemonType of string 

type poke_type = 
  | Bug | Dark | Dragon | Electric | Fairy | Fighting | Fire | Flying | Ghost 
  | Grass | Ground | Ice | Normal | Poison | Psychic | Rock | Steel | Water 

type move_name = string
type move = {
  move_type: poke_type;
  move_name: move_name;
}

type stats = {
  level: int;
  base_hp: int;
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
  base_hp = j |> member "base_hp" |> to_int;
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
  | "Fairy" -> Fairy
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

let string_from_type = function 
  | Bug -> "Bug"
  | Dark -> "Dark"
  | Dragon -> "Dragon"
  | Electric -> "Electric"
  | Fairy -> "Fairy"
  | Fighting -> "Fighting"
  | Fire -> "Fire"
  | Flying -> "Flying"
  | Ghost -> "Ghost"
  | Grass -> "Grass"
  | Ground -> "Ground"
  | Ice -> "Ice"
  | Normal -> "Normal"
  | Poison -> "Poison"
  | Psychic -> "Psychic"
  | Rock -> "Rock"
  | Steel -> "Steel"
  | Water -> "Water"

(** [moves_of_json j] is the pokemon move represented by [j] *)
let moves_of_json j = {
  move_type = j |> member "move_type" |> to_string |> type_from_string;
  move_name = j |> member "move_name" |> to_string; 
}

(** [poke_from_json j] is the pokemon represented by [j]. *)
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

(** [damage_multiplier t1 t2] is the multiplier when a move of type 
    [t2] is inflicted on a pokemon of type [t1]. *)
let damage_multiplier t1 t2 = 
  match t1 with
  | Bug -> begin 
      match t2 with 
      | Grass | Fighting | Ground -> 0.5
      | Fire | Flying | Rock -> 2.
      | _ -> 1. 
    end
  | Dark -> begin 
      match t2 with 
      | Psychic -> 0.
      | Ghost | Dark -> 0.5
      | Fighting | Bug | Fairy -> 2.
      | _ -> 1. 
    end
  | Dragon -> begin 
      match t2 with 
      | Fire | Water | Electric | Grass -> 0.5
      | Ice | Dragon | Fairy -> 2.
      | _ -> 1. 
    end
  | Electric -> begin 
      match t2 with 
      | Electric | Flying | Steel -> 0.5
      | Ground -> 2.
      | _ -> 1. 
    end
  | Fighting -> begin 
      match t2 with 
      | Bug | Rock | Dark -> 0.5
      | Flying | Psychic | Fairy -> 2.
      | _ -> 1. 
    end
  | Fairy -> begin 
      match t2 with 
      | Dragon -> 0.
      | Fighting | Bug | Dark -> 0.5
      | Poison | Steel -> 2.
      | _ -> 1. 
    end
  | Fire -> begin 
      match t2 with 
      | Fire | Grass | Ice | Bug | Steel | Fairy -> 0.5
      | Water | Ground | Rock -> 2.
      | _ -> 1. 
    end
  | Flying -> begin 
      match t2 with 
      | Ground -> 0.
      | Grass | Fighting | Bug -> 0.5
      | Electric | Ice | Rock -> 2.
      | _ -> 1. 
    end
  | Ghost -> begin 
      match t2 with 
      | Normal | Fighting -> 0.
      | Poison | Bug -> 0.5
      | Ghost | Dark -> 2.
      | _ -> 1. 
    end
  | Grass -> begin 
      match t2 with 
      | Water | Electric | Grass | Ground -> 0.5
      | Fire | Ice | Poison | Flying | Bug -> 2.
      | _ -> 1. 
    end
  | Ground -> begin 
      match t2 with 
      | Electric-> 0.
      | Poison | Rock -> 0.5
      | Water | Grass | Ice -> 2.
      | _ -> 1. 
    end
  | Ice -> begin 
      match t2 with 
      | Ice -> 0.5
      | Fire | Fighting | Rock | Steel -> 2.
      | _ -> 1. 
    end
  | Normal -> begin 
      match t2 with 
      | Ghost -> 0.
      | Fighting -> 2.
      | _ -> 1. 
    end
  | Poison -> begin 
      match t2 with 
      | Grass | Fighting | Poison | Bug | Fairy -> 0.5
      | Ground | Psychic -> 2.
      | _ -> 1. 
    end
  | Psychic -> begin 
      match t2 with 
      | Fighting | Psychic -> 0.5
      | Bug | Ghost | Dark -> 2.
      | _ -> 1. 
    end
  | Rock -> begin 
      match t2 with 
      | Normal | Fire | Poison | Flying -> 0.5
      | Water | Grass | Fighting | Ground | Steel -> 2.
      | _ -> 1. 
    end
  | Steel -> begin 
      match t2 with 
      | Normal | Grass | Ice | Flying | Psychic | Bug | Rock | Dragon
      | Steel | Fairy -> 0.5
      | Fire | Fighting | Ground  -> 2.
      | Poison -> 0.
      |_ -> 1.
    end 
  | Water -> begin 
      match t2 with 
      | Fire | Water | Ice | Steel -> 0.5
      | Electric | Grass ->  2.
      | _ -> 1. 
    end 

let attack_effectiveness pkm1 pkm2 attack =  
  let effectiveness = 
    match damage_multiplier pkm1.poke_type attack.move_type with 
    | 2. -> "It was super effective!"
    | 1.0 -> ""
    | 0.5 -> "It was not very effective..."
    | 0.0 -> "The move had no effect."
    | _ -> "" in 
  String.uppercase_ascii pkm2.name ^ " used " 
  ^ String.uppercase_ascii attack.move_name ^ " on " ^ 
  String.uppercase_ascii pkm1.name ^ ". " ^ 
  effectiveness 

let battle_damage pkm1 pkm2 move = 
  let damage_multiplier = damage_multiplier pkm1.poke_type move.move_type in 
  let pkm_lv = float_of_int pkm1.stats.level in 
  let pkm_defense = float_of_int pkm1.stats.defense in 
  let opp_attack = float_of_int pkm2.stats.attack in 
  let damage_float = 
    4.0 *. (((2. *. pkm_lv) /. 5.) *. (opp_attack /. pkm_defense))
    *. damage_multiplier in 
  let damage_int = int_of_float damage_float in 
  let dec_hp = pkm1.stats.hp - damage_int in 
  if dec_hp < 0 then 
    let new_stats = {pkm1.stats with hp = 0} in 
    {pkm1 with stats = new_stats} 
  else 
    let new_stats = {pkm1.stats with hp = dec_hp} in 
    {pkm1 with stats = new_stats}

let level_up pokemon = 
  let curr_stats = pokemon.stats in
  if curr_stats.curr_exp >= curr_stats.level_up_exp then 
    let new_stats = {
      level = curr_stats.level + 1;
      base_hp = curr_stats.base_hp + 10;
      hp = curr_stats.hp + 10;
      attack = curr_stats.attack + 15;
      defense = curr_stats.defense + 10;
      curr_exp = curr_stats.level_up_exp - curr_stats.curr_exp;
      level_up_exp = curr_stats.level_up_exp + 2;
    } in { pokemon with stats = new_stats;}
  else pokemon

let increase_exp p1 p2 = 
  let p2_lvl = float_of_int p2.stats.level in 
  let exp = int_of_float (p2_lvl *. 1.25) in 
  let curr_stats = p1.stats in 
  let new_stats = 
    { curr_stats with curr_exp = curr_stats.curr_exp + exp; } in 
  {p1 with stats = new_stats} 