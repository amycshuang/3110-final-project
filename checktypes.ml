module type AuthorsSig = sig
  val hours_worked : int
end

module AuthorsCheck : AuthorsSig = Author

module type PlayerSig = sig
  type t
  type nickname = string
  val init_player : nickname -> Pokemon.t -> t
  val catch_poke : t -> Pokemon.t -> t
  val get_poke_list : t -> (Pokemon.name * Pokemon.level) list
end 
module PlayerCheck : PlayerSig = Player

module type PokemonSig = sig 
  type t
  type name = string
  type poke_type
  type level = int
  type hp = int
  type attack = int
  type defense = int
  type curr_exp = int
  type level_up_exp = int 
  type caught = bool
  type move 
  type move_name = string
  exception InvalidPokemon of string
  val poke_from_json : Yojson.Basic.t -> t
  val get_name : t -> name
  val get_poke_type : t -> poke_type
  val get_level : t -> level
  val get_hp : t -> hp
  val get_attack : t -> attack
  val get_defense : t -> defense
  val get_curr_exp: t -> curr_exp
  val get_level_up_exp : t -> level_up_exp
  val get_caught : t -> caught
  val get_move : t -> move_name -> move
  val valid_move_name : t -> string -> bool
  val type_from_string : string -> poke_type
  val level_up : t -> t
  val increase_exp : t -> t -> t
end
module PokemonCheck : PokemonSig = Pokemon

module type CommandSig = sig 
  type phrase = string list
  type command = 
    | Quit
    | Yes
    | No
    | Run 
    | Catch
    | Bag 
    | Attack of phrase
    | Battle of phrase
  exception InvalidAttack of string
  exception InvalidCatch of string
  exception InvalidCommand of string
  val parse : Pokemon.t option -> string -> command
end 

module CommandCheck : CommandSig = Command