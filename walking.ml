open State
open Player
open Pokemon

type move = Up | Left | Right | Down

type display = Default | Bag | PokeList

type action = Move of move | Display of display

let walk_key ch =
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

let process_walk input st =
  let action = walk_key input in
  match action with
  | Move dir -> begin 
      let mv_st =  {st with player=(move_map st.player dir st.map)} in 
      {mv_st with status = (update_status (player_block mv_st.player mv_st.map))}
    end 
  | Display x -> {st with panel_txt=(display st x)}