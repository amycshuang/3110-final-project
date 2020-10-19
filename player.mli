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

