open Pokemon
open Player
open Block
open Graphics
open Trainer
open Yojson

type map = block array array
type attack_moves = {
  player_attack : Pokemon.move; 
  opponent_attack : Pokemon.move;
  battling_poke : Pokemon.pokemon array;
}

type menu = Default 
          | Fight 
          | PokeList 
          | Bag 
          | Run 
          | Catch 
          | Heal 
          | Switch 
          | Attack of attack_moves

type menu_state = {
  status : menu;
  player : Player.player;
  opponent: Pokemon.pokemon list;
  hover: int;
  select: menu option;
  p_turn : bool;
  previous: menu_state option
}

type status =  Walking 
            | WalkingGym
            | EnterGym
            | ExitGym
            | PokeCenter
            | Menu of menu_state
            | Win

type state = {
  maps : map array;
  player : Player.player;
  panel_txt : string;
  status : status;
  trainers: trainer array;
}

let get_key () = (wait_next_event [Key_pressed]).Graphics.key

let spawn_status block (st : state) = 
  let spawned = spawn_poke block in
  match spawned with
  | Some x -> let (mst : menu_state) = 
                {status = Default;
                 player = st.player; 
                 opponent = [x]; 
                 hover = 0; 
                 select = None;
                 p_turn = true;
                 previous = None
                } in 
    Menu mst
  | None -> Walking

let trainer_battle_status st = failwith "TODo"

let update_status (st : state) = function 
  | TallGrass -> spawn_status TallGrass st
  | Water -> spawn_status Water st
  | Grass -> Walking
  | Road | House -> Walking
  | Gym -> EnterGym
  | Null | GymRoad | BrownGymFloor | GreyGymFloor -> WalkingGym 
  | Exit -> ExitGym
  | PokeCenter -> PokeCenter 
  | Trainer | ClarksonSpot-> WalkingGym
(* let (mst : menu_state) = 
   {status = Default;
   player = st.player; 
   opponent = (battle_trainer st).poke_list; 
   hover = 0; 
   select = None;
   p_turn = true;
   previous = None
   } in 
   Menu mst *)

(* WalkingGym  *)
(** TODO: link with menu state start battle *)
(* | ClarksonSpot ->  *)



(* WalkingGym  *)
(** TODO: link with menu state start battle *)

(* let get_opponent opp = match opp with  
   | OppPokemon pkm -> pkm
   | OppTrainer -> failwith "TODO after initializing trainer module" *)

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
    maps = [|map; Block.json_to_map "map_jsons/gym_map.json"|];
    player = make_player name starter map;
    panel_txt = "Use your WASD keys to move around the map";
    status = Walking;
    trainers = trainer_array (Yojson.Basic.from_file "trainers.json");
  }