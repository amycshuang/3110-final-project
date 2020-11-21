(** The status of the game. *)
type status =  Walking 
            | Battling 
            | Encounter of Block.block 
            | Enter of Block.block 
            | Win

(** The type representing a map. *)
type map = Block.block array array

(** The type representing a player state. *)
type state = {
  map : map;
  player : Player.player;
  panel_txt : string;
  status : status;
}

(** The type representing an encounter state *)
type encounter_state = {
  player : Player.player;
  opponent: Pokemon.pokemon
}

(** [get_key ()] returns the corresponding character of the key pressed *)
val get_key : unit -> char

(** [update_status block] is the status associated with the block [block]. *)
val update_status : Block.block -> status

(** [player_block p map] is the block that the player [p] is standing on in 
    the map [map]. *)
val player_block : Player.player -> map -> Block.block

(** [init_state name starter map] is the state created with player name [name],
    starter pokemon [starter], and map [map]. *)
val init_state : string -> Pokemon.pokemon -> map -> state
