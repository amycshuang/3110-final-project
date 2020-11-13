(** The status of the game. *)
type status =  Walking 
            | Battling 
            | Encounter of Block.block 
            | Enter of Block.block 
            | Win

(** The type representing a map. *)
type map = Block.block array array

type state = {
  map : map;
  player : Player.player;
  panel_txt : string;
  status : status;
}

type encounter_state = {
  player : Player.player;
  opponent: Pokemon.pokemon
}

(** [get_key ()] returns the corresponding character of the key pressed *)
val get_key : unit -> char

val update_status : Block.block -> status

val player_block : Player.player -> map -> Block.block

val init_state : Player.nickname -> Pokemon.pokemon -> state
