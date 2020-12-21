(** 
   Representation of a game state.
*)

(** The type of values representing attack_moves. *)
type attack_moves = {
  player_attack : Pokemon.move; 
  opponent_attack : Pokemon.move;
  battling_poke : Pokemon.pokemon array;
}

(** The type of values representing the menu. *)
type menu = Default 
          | Fight 
          | PokeList 
          | Bag 
          | Run 
          | Catch 
          | Heal 
          | Switch 
          | Attack of attack_moves

(** The type of a menu_state. *)
type menu_state = {
  status : menu;
  player : Player.player;
  opponent : Pokemon.pokemon list;
  hover : int;
  select : menu option;
  is_trainer : bool
}

(** The status of the game. *)
type status =  Walking 
            | WalkingGym
            | EnterGym
            | ExitGym
            | PokeCenter
            | AlreadyBattled
            | CannotBattle
            | TrainerTalk of Trainer.trainer
            | TrainerOver 
            | Menu of menu_state
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

(** [player_block p map] is the block that the player [p] is standing on in 
    the map [map]. *)
val player_block : Player.player -> map -> Block.block

(** [init_state name starter map] is the state created with player name [name],
    starter pokemon [starter], and map [map]. *)
val init_state : string -> Pokemon.pokemon -> map -> state
