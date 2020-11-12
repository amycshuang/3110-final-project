open Pokemon
open Player
open Graphics

type status = Intro | Walking | Battling | Win

type block = TallGrass 
           | Water
           | Grass
           | Road
           | Gym
           | PokeCenter
           | House

type move = Up | Left | Right | Down

let get_key () = (wait_next_event [Key_pressed]).Graphics.key

type map = block array array

let map_key ch : move option =
  match ch with
  | 'w' -> Some Up
  | 'a' -> Some Left
  | 's' -> Some Down
  | 'd' -> Some Right
  | _ -> None

let move_map p m =
  let (x, y) = get_loc p in
  match m with
  | Some Up -> set_loc p (x, y + 1)
  | Some Left -> set_loc p (x - 1, y)
  | Some Down -> set_loc p (x, y - 1)
  | Some Right -> set_loc p (x + 1, y)
  | None -> p

let pikachu = poke_from_json (Yojson.Basic.from_file "pikachu.json")

let player_start blocks = 
  let ncol = Array.length blocks.(0) in
  let nrow = Array.length blocks in
  (nrow / 2, ncol / 2)

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

let player_block p map = 
  let (x, y) = get_loc p in
  (map.(x)).(y)