(** The status of the game. *)
type status = Intro | Walking | Battling | Win

(** The different types of blocks within a map. *)
type block = TallGrass 
           | Water
           | Grass
           | Road
           | Gym
           | PokeCenter
           | House

(** The type representing a map. *)
type map = block array array

val test_player : Player.t

(** TODO - change string to Pokemon.t after add initialize starter implementation *)
val make_player : string -> string -> Player.t

(** [get_key ()] returns the corresponding character of the key pressed *)
val get_key : unit -> char

(** [map_key ch] maps pressed character to an option of action*)
val process_input : char -> Player.t -> Player.t

(** [test_player] is a player used for testing to render *)
val test_player : Player.t

(** [trying] is a test map to test for rendering *)
val trying : map