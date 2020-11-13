module type AuthorsSig = sig
  val hours_worked : int
end
module AuthorsCheck : AuthorsSig = Author

module type PlayerSig = sig
  type nickname = string
  type badge = string
  type item = Potion | Pokeball
  type bag = {
    pc_box : Pokemon.pokemon list;
    badge_case : badge list;
    inventory : (item * int) list;
  }
  type player = {
    nickname : nickname;
    location : int * int;
    poke_list : Pokemon.pokemon list;
    bag : bag;
    balance : int;
  }
  val init_player : nickname -> Pokemon.pokemon -> int * int -> player
  val catch_poke : player -> Pokemon.pokemon -> player
end 
module PlayerCheck : PlayerSig = Player

module type PokemonSig = sig 
  type poke_type = 
    | Bug | Dark | Dragon | Electric | Fighting | Fire | Flying | Ghost 
    | Grass | Ground | Ice | Normal | Poison | Psychic | Rock | Steel | Water
  type move = {
    move_type: poke_type;
    move_name: string;
  }
  type stats = {
    level: int;
    hp: int;
    attack: int;
    defense: int;
    curr_exp: int;
    level_up_exp: int;
  }
  type pokemon = {
    name: string;
    poke_type: poke_type;
    stats: stats;
    caught: bool;
    move_set: move list
  }
  exception InvalidPokemon of string
  val poke_from_json : Yojson.Basic.t -> pokemon
  val poke_list_from_json : Yojson.Basic.t -> pokemon list
  val valid_move_name : pokemon -> string -> bool
  val type_from_string : string -> poke_type
  val level_up : pokemon -> pokemon
  val increase_exp : pokemon -> pokemon -> pokemon
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
    | Map of string
    | SPokemon of string
    | Attack of phrase
    | Battle of phrase
  exception InvalidRegion
  exception InvalidCommand of string
  val parse_region : phrase -> command
  val parse_starter: phrase -> command
  val parse_yn : phrase -> command 
  val parse : string -> (phrase -> command) -> command
end 
module CommandCheck : CommandSig = Command

module type BlockSig = sig 
  type block = 
    | TallGrass 
    | Water 
    | Road
    | Grass 
    | Gym 
    | House 
    | PokeCenter
  type block_type = string
  type map_dimensions = {
    width : int;
    height : int;
  }
  exception InvalidBlock of block_type
  val map_dim : Yojson.Basic.t -> map_dimensions
  val string_to_block : string -> block
  val json_to_list : Yojson.Basic.t -> string list
  val list_to_blocks : block_type list -> block list
  val json_to_map : string -> block array array
  val rev_matrix : block array -> block array
  val rev_matrix : block array -> block array
  val get_block_type : block -> block_type
  val poke_rand : Pokemon.pokemon list -> Pokemon.pokemon
  val spawn_poke : block -> Pokemon.pokemon option
end
module BlockCheck : BlockSig = Block

module type EncounterSig = sig 
  val process_encounter :  char -> State.state -> State.state
end 
module EncounterCheck : EncounterSig = Encounter

module type GuiSig = sig 
  val render_walk : State.state -> unit
  val render_encounter : State.state -> State.encounter_state -> unit
end 
module GuiCheck : GuiSig = Gui

module type InitialSig = sig 
  val initialize : State.state
end 
module InitialCheck : InitialSig = Initial

module type MainSig = sig 
  val main : unit -> unit
end 
module MainCheck : MainSig = Main

module type StateSig = sig 
  type status =  Walking 
              | Battling 
              | Encounter of Block.block 
              | Enter of Block.block 
              | Win
  type map = Block.block array array
  type state = {
    map : map;
    player : Player.player;
    panel_txt : string;
    status : status;
  }
  type encounter_state = {
    player : Player.player;
    opponent: Pokemon.pokemon
  }
  val get_key : unit -> char
  val update_status : Block.block -> status
  val player_block : Player.player -> map -> Block.block
  val init_state : Player.nickname -> Pokemon.pokemon -> map -> state
end 
module StateCheck : StateSig = State

module type WalkingSig = sig 
  val process_walk : char -> State.state -> State.state
end 
module WalkingCheck : WalkingSig = Walking



