type attack_moves = {
  player_attack : Pokemon.move; 
  opponent_attack : Pokemon.move;
  battling_poke : Pokemon.pokemon array;
}

type menu = Default 
          | Fight 
          | PokeList 
          | Bag 
          | Run 
          | Catch 
          | Heal 
          | Switch 
          | Attack of attack_moves

type menu_state = {
  status : menu;
  player : Player.player;
  opponent: Pokemon.pokemon list;
  hover: int;
  select: menu option;
  p_turn : bool;
  previous: menu_state option
}

(** The status of the game. *)
type status =  Walking 
            | WalkingGym
            | PokeCenter 
            | Menu of menu_state
            (* | Gym *)
            | Win

(** The type representing a map. *)
type map = Block.block array array

(** The type representing a player state. *)
type state = {
  maps : map array;
  player : Player.player;
  panel_txt : string;
  status : status;
  trainers: Trainer.trainer list;
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
