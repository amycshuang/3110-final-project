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

type battle = Begin 
            | Battling 
            | Over 
            | CannotBattle

type menu_state = {
  status : menu;
  player : Player.player;
  <<<<<<< HEAD
    opponent : Pokemon.pokemon list;
  hover : int;
  select : menu option;
  is_trainer : bool;
  previous : menu_state option
             =======
             opponent: Pokemon.pokemon list;
  hover: int;
  select: menu option;
  previous: menu_state option
    >>>>>>> origin/kassie
}

(** The status of the game. *)
type status =  Walking 
            | WalkingGym
            | EnterGym
            | ExitGym
            | PokeCenter
            | TrainerTalk  
            | TrainerOver 
            | Menu of menu_state
            | Win

type state = {
  maps : map array;
  player : Player.player;
  panel_txt : string;
  status : status;
  trainers: trainer list;
}

let get_key () = (wait_next_event [Key_pressed]).Graphics.key

(** TODO: add comment *)
let spawn_status block (st : state) = 
  let spawned = spawn_poke block in
  match spawned with
  | Some x -> let (mst : menu_state) = 
                {status = Default;
                 player = st.player; 
                 opponent = [x]; 
                 hover = 0; 
                 select = None;
                 is_trainer = false;
                 previous = None
                } in 
    Menu mst
  | None -> Walking

(** TODO: add comment *)
let trainer_battle st =   
  match List.filter (fun (t : Trainer.trainer) -> 
      (t.x, t.y) = st.player.location) st.trainers with 
  | [] -> failwith "impossible"
  | h :: t -> h 

let update_status (st : state) = function 
  | TallGrass -> spawn_status TallGrass st
  | Water -> spawn_status Water st
  | Grass -> Walking
  | Road | House -> Walking
  | Gym -> EnterGym
  | Null | GymRoad | BrownGymFloor | GreyGymFloor -> WalkingGym 
  | Exit -> ExitGym
  | PokeCenter -> PokeCenter 
  | Trainer | ClarksonSpot -> TrainerTalk
(* let mst = { status = Default;
            player = st.player; 
            opponent = trainer.poke_list; 
            hover = 0; 
            select = None;
            p_turn = true;
            previous = None
          } in 
   if trainer = (List.hd (List.rev st.trainers)) then 
   let gym_state = {
    trainer = trainer; 
    battle = mst; 
    status = Begin;
   }
   in GymBattle gym_state
   else let gym_state = {
    trainer = trainer; 
    battle = mst; 
    status = CannotBattle;
   }
   in GymBattle gym_state *)

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