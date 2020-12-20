(** 
   Representation of player data.

   This module represents that data stored in player files, including their
   bag, location, and balance. 
*)

(** The type of items. *)
type item = Potion | Pokeball

(** The type of a player's bag. *)
type bag = {
  (* pc_box : Pokemon.pokemon list; *)
  badge_case : string list;
  inventory : (item * int) list;
}

(** The type of values representing a player. *)
type player = {
  nickname : string;
  location : int * int;
  poke_list : Pokemon.pokemon list;
  bag : bag;
  balance : int;
}

(** [init_player name start_poke] creates and initializes a player with a 
    nickname and a starter pokemon. *)
val init_player : string -> Pokemon.pokemon -> int * int -> player

(** [catch_poke player poke] catches a Pokemon and adds it to the player's
    Pokemon party or PC box. *)
val catch_poke : player -> Pokemon.pokemon -> player
