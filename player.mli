(** 
   Representation of player data.

   This module represents that data stored in player files, including their
   bag, location, and balance. 
*)

type nickname = string
type badge = string
type item = Potion | Pokeball

(** The type of a player's bag. *)
type bag = {
  pc_box : Pokemon.t list;
  badge_case : badge list;
  inventory : (item * int) list;
}

(** The type of values representing a player. *)
type player = {
  nickname : nickname;
  location : int * int;
  poke_list : Pokemon.t list;
  bag : bag;
  balance : int;
}

(** [init_player name start_poke] creates and initializes a player with a 
    nickname and a starter pokemon. *)
val init_player : nickname -> Pokemon.t -> int * int -> player

(** [catch_poke player poke] catches a Pokemon and adds it to the player's
    Pokemon party or PC box. *)
val catch_poke : player -> Pokemon.t -> player
