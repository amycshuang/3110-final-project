(** 
   Representation of a Pokecamel game state.
*)
open Pokemon
open Player

(** The abstract type of values representing the game state. *)
type t

(** The status of the player in the game, representing if the player has 
    defeated the gym leader yet *)
type status

(** Raised if a move is invalid. *)
exception InvalidMove

(** [init_state] is the initial state of the game. 
    ex. initializes map and blocks, initializes player, etc. *)
val init_state : t

(** [get_player st] is the current data of the player. *)
val get_player : t -> Player.t

(** [update_player_bag st] is the player after their bag has been updated. *)
val update_player_bag : t -> Player.t

(** [string_of_player_data pl] prints the player's current data. *)
val print_player_data : Player.t -> unit

(** [move_player st poke] is the data of the pokemon the player chooses 
    from their pokemon list *)
val choose_pokemon : t -> Pokemon.name -> Pokemon.t