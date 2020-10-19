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
val init_player : nickname -> Pokemon.t -> t

(** [check_pc player] ensures the player only has 6 pokemon in the player's 
    party, otherwise, extra pokemon are moved to the PC box. *)
val check_pc : t -> t

(** [catch_poke player poke] catches a Pokemon and adds it to the player's
    Pokemon party or PC box. *)
val catch_poke : t -> Pokemon.t -> t