(**
   Representation of a block on the map.
*)

(** The type of values representing a block on the map. *)
type block = 
  | TallGrass 
  | Water 
  | Road
  | Grass 
  | Gym 
  | House 
  | PokeCenter

(** The type of the block represented as a string. *)
type block_type = string

(** The type of the map's dimensions. *)
type map_dimensions = {
  width : int;
  height : int;
}

(** Raised when there is an invalid block type in a map. *)
exception InvalidBlock of block_type

(** [map_dim json] is the map dimensions of the map [json] 
    Requires: [json] is a valid JSON map representation. *)
val map_dim : Yojson.Basic.t -> map_dimensions

(** [string_to_block s] converts a string to type t.
    Raises [InvalidBlock s] if [s] is not a valid block. *)
val string_to_block : string -> block

(** [json_to_list json] is the map that [json] represents. *)
val json_to_list : Yojson.Basic.t -> string list

(** [list_to_blocks lst] converts a list of block_types to a list of t types. *)
val list_to_blocks : block_type list -> block list

(** [list_to_matrix lst json] converts a list [lst] of elements of type t to a 
    2d array of a size specificed by the JSON [json]. *)
val list_to_matrix : block list -> Yojson.Basic.t -> block array array

(** [rev_matrix a] reverses the array [a]. *)
val rev_matrix : block array -> block array

(** [get_block_type t] returns the block_type representation of type t. *)
val get_block_type : block -> block_type

(** [poke_list_from_json json] is the a list of pokemon from the JSON [json]. *)
val poke_list_from_json : Yojson.Basic.t -> Pokemon.t list

(** [spawn_poke lst r] is the pokemon at a random index from a list [lst] of 
    pokemon.*)
val spawn_poke : Pokemon.t list -> Pokemon.t

(** [spawn_prob t lst] decides whether Some pokemon from the pokemon list [lst] 
    will spawn on a block [t]. If a pokemon is not spawned, None is returned. *)
val spawn_prob : block -> Pokemon.t list -> Pokemon.t option