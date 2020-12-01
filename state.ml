open Pokemon
open Player
open Block
open Graphics

type menu = Fight | PokeList | Bag | Run

(** The type representing an encounter state *)
type menu_state = {
  player : Player.player;
  opponent: Pokemon.pokemon;
  hover: int;
  select: menu option
}

type encounter_state = {
  player : Player.player;
  opponent: Pokemon.pokemon;
  hover: int;
  select: menu option       
}

type battle_state = {
  player : Player.player;
  opponent: Pokemon.pokemon;
  p_turn : bool;
} 

type status =  Walking 
            | Menu of menu_state
            | Battling of battle_state
            | Encounter of encounter_state 
            | Enter of block 
            | Win

type map = block array array

type state = {
  map : map;
  player : Player.player;
  panel_txt : string;
  status : status;
}

let get_key () = (wait_next_event [Key_pressed]).Graphics.key

let spawn_status block (st : state) = 
  let spawned = spawn_poke block in
  match spawned with
  | Some x -> let (mst : menu_state) = 
                {player = st.player; opponent = x; hover=0; select=None} in 
    Menu mst
  | None -> Walking

let update_status (st : state) = function 
  | TallGrass -> spawn_status TallGrass st
  | Water -> spawn_status Water st
  | Grass -> Walking
  | Road -> Walking
  | Gym -> Enter Gym
  | PokeCenter -> Enter PokeCenter
  | House -> Enter House

let player_block p map = 
  let (x, y) = p.location in 
  (map.(y)).(x)

(** [player_start blocks] is the player's location on the map. *)
let player_start blocks = 
  let ncol = Array.length blocks.(0) in
  let nrow = Array.length blocks in
  ((ncol / 2) - 1, nrow / 2)

(** [make_player name starter map] is the player made with name [name],
    starter pokemon [starter], and map [map]. *)
let make_player name starter map = init_player name starter (player_start map)

let init_state name starter map = 
  let start_game_message = 
    "Good choice " ^ name ^ "!\n" ^ 
    "Good luck on your adventure!\nPeace, love, and 3110" in
  ANSITerminal.(print_string [cyan] (start_game_message));
  print_endline "";
  {
    map = map;
    player = make_player name starter map;
    panel_txt = "Use your WASD keys to move around the map";
    status = Walking;
  }