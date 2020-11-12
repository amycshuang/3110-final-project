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

type state = {
  map : map;
  player : Player.t;
  panel_txt : string;
}

(** [get_key ()] returns the corresponding character of the key pressed *)
val get_key : unit -> char

(** [map_key ch] maps pressed character to an option of action*)
val process_input : char -> state -> state

(** [trying] is a test map to test for rendering *)
val testing_state : state