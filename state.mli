type block = TallGrass 
           | Water
           | Grass
           | Road
           | Gym
           | PokeCenter
           | House

type move

type map = block array array

val test_player : Player.t

val get_key : unit -> char

(**[map_key ch] maps pressed character to an option of action*)
val map_key : char -> move option

val move_map : Player.t -> move option -> Player.t

val trying : map