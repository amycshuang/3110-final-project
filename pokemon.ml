open Yojson.Basic.Util

type poke_type = 
  | Bug | Dark | Dragon | Electric | Fairy | Fighting | Fire | Flying | Ghost 
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

(** [damage_multiplier t1 t2] is the multiplier when a move of type 
    [t2] is inflicted on a pokemon of type [t1]. *)
let damage_multiplier t1 t2 = 
  match t1 with
  | Bug -> begin 
      match t2 with 
      | Fire | Fighting | Poison | Flying | Ghost | Steel | Fairy -> 0.5
      | Grass | Psychic | Dark -> 2.
      | _ -> 1. 
    end
  | Dark -> begin 
      match t2 with 
      | Poison | Dark | Fairy -> 0.5
      | Psychic | Ghost -> 2.
      | _ -> 1. 
    end
  | Dragon -> begin 
      match t2 with 
      | Fairy -> 0.
      | Steel -> 0.5
      | Dragon -> 2.
      | _ -> 1. 
    end
  | Electric -> begin 
      match t2 with 
      | Ground -> 0.
      | Electric | Grass | Dragon -> 0.5
      | Water | Flying -> 2.
      | _ -> 1. 
    end
  | Fighting -> begin 
      match t2 with 
      | Ghost -> 0.
      | Poison | Flying | Psychic | Bug | Fairy -> 0.5
      | Normal | Ice | Rock | Dark | Steel -> 2.
      | _ -> 1. 
    end
  | Fairy -> begin 
      match t2 with 
      | Fire | Poison | Steel -> 0.5
      | Fighting | Dragon | Dark -> 2.
      | _ -> 1. 
    end
  | Fire -> begin 
      match t2 with 
      | Fire | Water | Rock | Dragon -> 0.5
      | Grass | Ice | Bug | Steel -> 2.
      | _ -> 1. 
    end
  | Flying -> begin 
      match t2 with 
      | Electric | Rock | Steel -> 0.5
      | Grass | Fighting | Bug -> 2.
      | _ -> 1. 
    end
  | Ghost -> begin 
      match t2 with 
      | Normal -> 0.
      | Dark -> 0.5
      | Psychic | Ghost -> 2.
      | _ -> 1. 
    end
  | Grass -> begin 
      match t2 with 
      | Fire | Grass | Poison | Flying | Bug | Dragon | Steel -> 0.5
      | Water | Ground | Rock -> 2.
      | _ -> 1. 
    end
  | Ground -> begin 
      match t2 with 
      | Flying -> 0.
      | Grass | Bug -> 0.5
      | Fire | Electric | Poison | Rock | Steel -> 2.
      | _ -> 1. 
    end
  | Ice -> begin 
      match t2 with 
      | Fire | Water | Ice | Steel -> 0.5
      | Grass | Ground | Flying | Dragon -> 2.
      | _ -> 1. 
    end
  | Normal -> begin 
      match t2 with 
      | Ghost -> 0.
      | Rock | Steel -> 0.5
      | _ -> 1. 
    end
  | Poison -> begin 
      match t2 with 
      | Steel -> 0.
      | Poison | Rock | Ground | Ghost -> 0.5
      | Grass | Fairy -> 2.
      | _ -> 1. 
    end
  | Psychic -> begin 
      match t2 with 
      | Dark -> 0.
      | Psychic | Steel -> 0.5
      | Fighting | Poison -> 2.
      | _ -> 1. 
    end
  | Rock -> begin 
      match t2 with 
      | Fighting | Ground | Steel -> 0.5
      | Fire | Ice | Flying | Bug -> 2.
      | _ -> 1. 
    end
  | Steel -> begin 
      match t2 with 
      | Normal | Grass | Ice | Flying | Psychic | Bug |
        Rock | Dragon | Steel | Fairy -> 0.5
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

let battle_damage pkm1 pkm2 move = 
  let damage_multiplier = damage_multiplier pkm1.poke_type move.move_type in 
  let pkm_lv = float_of_int pkm1.stats.level in 
  let pkm_defense = float_of_int pkm1.stats.defense in 
  let opp_attack = float_of_int pkm2.stats.attack in 
  let damage_float = (((2. *. pkm_lv) /. 5.) *. (opp_attack /. pkm_defense))
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
      base_hp = curr_stats.base_hp + 2;
      hp = curr_stats.hp + 2;
      attack = curr_stats.attack + 2;
      defense = curr_stats.defense + 2;
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