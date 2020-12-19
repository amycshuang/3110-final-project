open Pokemon
open Yojson.Basic.Util

(** The type of values representing a trainer. *)
type trainer = {
  name : string;
  catchphrase : string;
  poke_list : Pokemon.pokemon list;
}

let trainer_from_json j = {
  name = j |> member "name" |> to_string;
  catchphrase = j |> member "catchphrase" |> to_string;
  poke_list = j |> member "poke_list" |> Pokemon.poke_list_from_json;
} 

(** [trainer_list_from_json j] is a list of trainers from the JSON [j].
    Requires: [j] is a valid JSON for a list of trainers. *)
let trainer_list_from_json j = j |> to_list |> List.map trainer_from_json



(* let random_trainer_poke t = 
   let i = Random.int 4 in
   List.nth t.poke_list i

   let pick_trainer_poke t = 
   let poke = List.hd t.poke_list *)