open Yojson.Basic.Util

type map_dimensions = {
  width : int;
  height : int;
}

type block_type = string

exception InvalidBlock of block_type

type block = 
  | TallGrass 
  | Water 
  | Road
  | Grass 
  | Gym 
  | House 
  | PokeCenter
  | Null
  | GymRoad
  | BrownGymFloor
  | GreyGymFloor
  | Exit
  | Trainer
  | ClarksonSpot

let map_dim (json : Yojson.Basic.t) = {
  width = json |> member "width" |> to_int;
  height = json |> member "height" |> to_int;
}

let string_to_block (s : block_type) =
  match s with
  | "tall grass" -> TallGrass
  | "grass" -> Grass
  | "water" -> Water
  | "road" -> Road
  | "gym" -> Gym
  | "house" -> House
  | "pokecenter" -> PokeCenter
  | "null" -> Null
  | "gym road" -> GymRoad
  | "brown gym floor" -> BrownGymFloor
  | "grey gym floor" -> GreyGymFloor
  | "exit" -> Exit
  | "trainer" -> Trainer
  | "clarkson spot" -> ClarksonSpot
  |  _ -> raise (InvalidBlock s)

let json_to_list json = 
  [json]
  |> filter_member "blocks"
  |> flatten
  |> filter_member "type"
  |> filter_string

let list_to_blocks lst =
  List.map (string_to_block) lst

let json_to_map (j : string) = 
  let json = Yojson.Basic.from_file j in
  let json_lst = json_to_list json in
  let block_list = list_to_blocks json_lst in
  let arr = Array.of_list block_list in
  let dim = map_dim json in
  let matrix = Array.make_matrix dim.height dim.width Grass in
  for i = 0 to (dim.height - 1) do
    for j = 0 to (dim.width - 1) do
      matrix.(i).(j) <- (Array.get arr (i * dim.width + j))
    done
  done;
  matrix  

let rev_matrix a =
  let len = Array.length a in
  for i = 0 to (len/2) do
    let temp = a.(i) in
    a.(i) <- a.(len-i-1);
    a.(len-i-1) <- temp
  done;
  a

let get_block_type (t : block) =
  match t with
  | TallGrass -> "tall grass"
  | Water -> "water"
  | Road -> "road"
  | Grass -> "grass"
  | Gym -> "gym"
  | House -> "house" 
  | PokeCenter -> "pokecenter"
  | Null -> "null"
  | GymRoad -> "gym road"
  | BrownGymFloor -> "brown gym floor"
  | GreyGymFloor -> "grey gym floor"
  | Exit -> "exit"
  | Trainer -> "trainer"
  | ClarksonSpot -> "clarkson spot "

(** [water_poke] is the pokemon list from the water_pokemon.json. This is a list
    of all the pokemon that can spawn on water blocks. *)
let water_poke = Pokemon.poke_list_from_json 
    (Yojson.Basic.from_file "water_pokemon.json")

(** [grass_poke] is the pokemon list from the grass_pokemon.json. This is a list
    of all the pokemon that can spawn on tall grass blocks. *)
let grass_poke = Pokemon.poke_list_from_json 
    (Yojson.Basic.from_file "grass_pokemon.json")

let poke_rand (lst : Pokemon.pokemon list) =
  let random = Random.int (List.length lst) in
  let poke_arr = Array.of_list lst in
  poke_arr.(random)

let spawn_poke (t : block) =
  let random = Random.int 3 in
  match t with
  | TallGrass -> 
    if random = 0 || random = 1 then Some (poke_rand grass_poke)
    else None
  | Water -> 
    if random = 0 || random = 1 then Some (poke_rand water_poke)
    else None
  | Road -> None
  | Grass -> None
  | Gym -> None
  | House -> None
  | PokeCenter -> None
  | Null -> None
  | GymRoad -> None
  | BrownGymFloor -> None
  | GreyGymFloor -> None
  | Exit -> None
  | Trainer -> None
  | ClarksonSpot -> None