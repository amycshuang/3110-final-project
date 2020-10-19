(* Note: You may introduce new code anywhere in this file. *) 
open Pokemon

type nickname = string
type badge = string
type item = Potion | Pokeball

(** The type of a player's bag. *)
type bag = {
  pc_box : Pokemon.t list;
  badge_case : badge list;
  inventory : item list;
}

(** The type of values representing a player. *)
type player = {
  nickname : nickname;
  starter : Pokemon.t;
  location : float * float;
  poke_list : Pokemon.t list;
  bag : bag;
  balance : int;
}

type t = player

(** [exits_of_json j] is the parsed adventure exit [j] represents.
    Requires: [j] is a valid JSON exit representation. *)
let init_player name start_poke = 
  failwith "unimplemented"


