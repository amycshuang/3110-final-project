(**
   Representation of a block on the map.
*)

(** Abstract type of values representing a block on the map. *)
type t

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
val string_to_block : string -> t

(** [json_to_list json] is the map that [json] represents. *)
val json_to_list : Yojson.Basic.t -> string list

(** [list_to_blocks] converts a list of block_types to a list of t types. *)
val list_to_blocks : block_type list -> t list

(** [get_block_type t] returns the block_type representation of type t. *)
val get_block_type : t -> block_type

(** [spawn_poke lst r] is the pokemon at a random index from a list [lst] of 
    pokemon.*)
val spawn_poke : Pokemon.t list -> Pokemon.t

(** [spawn_prob t lst] decides whether Some pokemon from the pokemon list [lst] 
    will spawn on a block [t]. If a pokemon is not spawned, None is returned. *)
val spawn_prob : t -> Pokemon.t option