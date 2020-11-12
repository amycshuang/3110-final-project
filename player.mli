(** 
   Representation of player data.

   This module represents that data stored in player files, including their
   bag, location, and balance. 
*)

(** The abstract type of values representing a player. *)
type t

(** The type of nickname. *)
type nickname = string

(** [init_player name start_poke] creates and initializes a player with a 
    nickname and a starter pokemon. *)
val init_player : nickname -> Pokemon.t -> int * int -> t

(** [catch_poke player poke] catches a Pokemon and adds it to the player's
    Pokemon party or PC box. *)
val catch_poke : t -> Pokemon.t -> t

(** [get_poke_list player] is a list of the names of the pokemon and their levels
    in a player's pokemon list. *)
val get_poke_list : t -> (Pokemon.name * Pokemon.level) list

(** getter functions:
    - bag, each list *)

(** [get_loc player] is a block on which the player is on. *)
val get_loc : t -> int * int

(** [set_loc player x] is the player with new location [x]. *)
val set_loc : t -> int * int -> t