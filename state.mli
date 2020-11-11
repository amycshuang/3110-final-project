type block = TallGrass 
           | Water
           | Grass
           | Road
           | Gym
           | PokeCenter
           | House

type move

val test_player : Player.t

val get_key : unit -> char

val map_key : char -> move option

val move_map : Player.t -> move option -> Player.t

val trying : block array array