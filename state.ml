open Pokemon
open Player
open Block
open Graphics

type status =  Walking | Battling | Encounter of block | Enter of block | Win

type move = Up | Left | Right | Down

type display = Default | Bag | PokeList

type action = Move of move | Display of display

let get_key () = (wait_next_event [Key_pressed]).Graphics.key

type map = block array array

type state = {
  map : map;
  player : Player.player;
  panel_txt : string;
  status : status;
}

let map_key ch =
  match ch with
  | 'w' -> Move Up
  | 'a' -> Move Left
  | 's' -> Move Down
  | 'd' -> Move Right
  | 'b' -> Display Bag
  | 'p' -> Display PokeList
  | _ -> Display Default

let check_bounds (x1, y1) (x2, y2) map = 
  let ncol = Array.length map.(0) in
  let nrow = Array.length map in
  if (x2 < 0 || x2 > (ncol - 1)) || (y2 < 0 || y2 > (nrow - 1)) 
  then (x1, y1) else (x2, y2)

let move_map p m map =
  let (x, y) = p.location in
  let new_loc = 
    match m with
    | Up -> (x, y + 1)
    | Left -> (x - 1, y)
    | Down -> (x, y - 1)
    | Right -> (x + 1, y)
  in {p with location=(check_bounds (x, y) new_loc map)}

let string_of_item = function
  | Potion -> "Potion"
  | Pokeball -> "Pokeball"

let parse_bag p = 
  let bag = p.bag in
  let inventory = bag.inventory in
  let rec parse_inventory = function
    | [] -> ""
    | (item, ct) :: t -> (string_of_item item) ^ ": " ^ string_of_int ct ^ "\n" 
                         ^ parse_inventory t in 
  parse_inventory inventory

let parse_pokelist p =
  let pokelist = p.poke_list in
  let rec parse_poke = function
    | [] -> ""
    | pokemon :: t -> (get_name pokemon) ^ "\n" ^ parse_poke t in
  parse_poke pokelist

let display st = function
  | Bag -> parse_bag st.player
  | PokeList -> parse_pokelist st.player
  | Default -> "Default txt"

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

let process_input input st =
  let action = map_key input in
  match action with
  | Move dir -> begin 
      let mv_st =  {st with player=(move_map st.player dir st.map)} in 
      {mv_st with status = (update_status (player_block mv_st.player mv_st.map))}
    end 
  | Display x -> {st with panel_txt=(display st x)}

let process_encounter input (st : state) = 
  let action = map_key input in 
  match action with 
  | Move dir -> begin 
      let mv_st =  {st with player=(move_map st.player dir st.map)} in 
      {mv_st with status = (update_status (player_block mv_st.player mv_st.map))}
    end 
  | Display x -> {st with panel_txt=(display st x)}

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

let init_state name starter = {
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
