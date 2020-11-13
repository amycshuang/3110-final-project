open Pokemon
open Player
open Block
open Graphics

type status =  Walking | Battling | Encounter of block | Enter of block | Win

let get_key () = (wait_next_event [Key_pressed]).Graphics.key

type map = block array array

type state = {
  map : map;
  player : Player.player;
  panel_txt : string;
  status : status;
}

let update_status = function 
  | TallGrass -> Encounter TallGrass
  | Water -> Encounter Water
  | Grass -> Walking
  | Road -> Walking
  | Gym -> Enter Gym
  | PokeCenter -> Enter PokeCenter
  | House -> Enter House

let player_block p map = 
  let (x, y) = p.location in 
  (map.(y)).(x)

let pikachu = poke_from_json (Yojson.Basic.from_file "pikachu.json")

let player_start blocks = 
  let ncol = Array.length blocks.(0) in
  let nrow = Array.length blocks in
  ((ncol / 2) - 1, nrow / 2)

let trying = [|[|Grass; Grass; Grass; Grass; Water; Water; Water; Road;
                 TallGrass; Grass; Grass; Grass; Grass; Grass; Grass; Grass;
                 Grass; TallGrass; TallGrass; TallGrass;|];
               [|Grass; House; Grass; Grass; Water; TallGrass; Water; Road; 
                 TallGrass; Grass; TallGrass; TallGrass; TallGrass; TallGrass;
                 TallGrass; Grass; Grass; TallGrass; TallGrass; TallGrass;|];
               [|Grass; Road; House; Grass; Water; Water; Water; Road;
                 TallGrass; Grass; TallGrass; TallGrass; TallGrass; Water;
                 TallGrass; Grass; Grass; TallGrass; TallGrass; TallGrass;|];
               [|TallGrass; Road; Road; Road; Road; Road; Road; Road;
                 Road; Road; Grass; TallGrass; Water; Water; Grass; Grass;
                 Grass; TallGrass; TallGrass; TallGrass|];
               [|TallGrass; Road; House; House; House; House; TallGrass; 
                 TallGrass; Road; Road; Road; Road; Road; Road; Road; 
                 PokeCenter; PokeCenter; Grass; Grass; Grass|];
               [|TallGrass; Road; Grass; Grass; Grass; Grass; TallGrass; 
                 TallGrass; Road; Road; Road; Road; Road; Road; Road; Road; 
                 Road; Road; Road; Road|];
               [|TallGrass; Road; Grass; Grass; Grass; Grass; Water; Water; Gym; Gym; Gym; Grass; TallGrass; Road; TallGrass; Road; Grass; Grass; Grass; Grass|];
               [|TallGrass; Road; TallGrass; TallGrass; TallGrass; Water; Water; Water; Gym; Gym; Gym; Grass; TallGrass; Road; TallGrass; Road; Water; TallGrass; TallGrass; TallGrass|];
               [|TallGrass; Road; TallGrass; TallGrass; TallGrass; Water; Water; Water; Grass; Road; Grass; Grass; TallGrass; Road; TallGrass; Road; Water; TallGrass; TallGrass; TallGrass|];
               [|TallGrass; Road; Road; Road; Road; Road; Road; Road; Road; 
                 Road; Road; Road; Road; Road; Road; Road; Water; TallGrass; 
                 TallGrass; TallGrass|]; 
               [|TallGrass; TallGrass; TallGrass; TallGrass; TallGrass; Road; 
                 TallGrass; TallGrass; Grass; Grass; House; Grass; Grass; House; 
                 Road; Road; Road; Water; Water; Water|];
               [|TallGrass; TallGrass; TallGrass; TallGrass;
                 TallGrass; Road; TallGrass; TallGrass; Grass; House; Grass;
                 Grass; Grass; Grass; House; Road; Road; Water; Water;
                 Water|]|]

let test_player = init_player "testing" pikachu (player_start trying)

(** TODO - change the starter to an actual pokemon object *)
let make_player name starter = init_player name pikachu (player_start trying)

let init_state name starter = 
  let start_game_message = 
    "Good choice " ^ name ^ "! " ^ 
    "Good luck on your adventure! Peace, love, and 3110" in
  ANSITerminal.(print_string [cyan] (start_game_message));
  print_endline "";
  {
    map = trying;
    player = make_player name starter;
    panel_txt = "default";
    status = Walking;
  }

let testing_state = {
  map = trying;
  player = test_player;
  panel_txt = "Default text";
  status = Walking;
}
