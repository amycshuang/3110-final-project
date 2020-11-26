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
  |  _ -> raise (InvalidBlock s)

let json_to_list json = 
  [json]
  |> filter_member "blocks"
  |> flatten
  |> filter_member "type"
  |> filter_string

let list_to_blocks lst =
  List.map (string_to_block) lst

let list_to_matrix (lst : block list) json = 
  let arr = Array.of_list lst in
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

let poke_list_from_json j = j |> to_list |> List.map Pokemon.poke_from_json

let spawn_poke (lst : Pokemon.t list) =
  let random = Random.int (List.length lst) in
  let poke_arr = Array.of_list lst in
  poke_arr.(random)

let spawn_prob (t : block) (lst : Pokemon.t list) =
  let random = Random.int 3 in
  match t with
  | TallGrass -> 
    if random = 0 || random = 1 then Some (spawn_poke lst)
    else None
  | Water -> 
    if random = 0 || random = 1 then Some (spawn_poke lst)
    else None
  | Road -> None
  | Grass -> None
  | Gym -> None
  | House -> None
  | PokeCenter -> None

(* let map = json_to_list (Yojson.Basic.from_file "map1.json")

   let dims = map_dim (Yojson.Basic.from_file "map1.json")


   let map_list = list_to_blocks map

   let map_arr = list_to_matrix map_list (Yojson.Basic.from_file "map1.json")

   (** To get the right array (starting from top left) *)
   let map_rev = rev_matrix map_arr

   let starter_poke = poke_list_from_json (Yojson.Basic.from_file "starter_pokemon.json") *)
