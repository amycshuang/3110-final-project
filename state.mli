type menu = Fight | PokeList | Bag | Run

(* type opponent = 
   | OppPokemon of Pokemon.pokemon list
   | OppTrainer *)

type menu_state = {
  player : Player.player;
  opponent: Pokemon.pokemon list;
  hover: int;
  select: menu option;
  (* is_encounter: bool; *)
}

type battle_state = {
  player: Player.player;
  opponent: Pokemon.pokemon list;
  p_turn: bool;
  hover: int;
  select: menu option 
}

(** The type representing an encounter state *)
(* type encounter_state = {
   player : Player.player;
   opponent: Pokemon.pokemon list;
   hover: int;
   select: menu option
   }

   type battle_state = {
   player : Player.player;
   opponent: Pokemon.pokemon list;
   p_turn : bool
   }  *)

(** The status of the game. *)
type status =  Walking 
            | PokeCenter 
            | Menu of menu_state
            | Battle of battle_state
            (* | Battling of battle_state
               | Encounter of encounter_state  *)
            | Gym
            | Win

(** The type representing a map. *)
type map = Block.block array array

(** The type representing a player state. *)
type state = {
  map : map;
  player : Player.player;
  panel_txt : string;
  status : status
}

(** [get_key ()] returns the corresponding character of the key pressed *)
val get_key : unit -> char

(** [update_status block] is the status associated with the block [block]. *)
val update_status : state -> Block.block -> status

(** [get_opponent opp] is the value carried by opponent [opp].  *)
(* val get_opponent : opponent -> Pokemon.pokemon list *)

(** [player_block p map] is the block that the player [p] is standing on in 
    the map [map]. *)
val player_block : Player.player -> map -> Block.block

(** [init_state name starter map] is the state created with player name [name],
    starter pokemon [starter], and map [map]. *)
val init_state : string -> Pokemon.pokemon -> map -> state
