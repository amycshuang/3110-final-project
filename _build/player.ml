open Pokemon

type nickname = string
type badge = string
type item = Potion | Pokeball

(** The type of a player's bag. *)
type bag = {
  pc_box : Pokemon.t list;
  badge_case : badge list;
  inventory : (item * int) list;
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
let empty_bag = {
  pc_box = [];
  badge_case = [];
  inventory = [];
}

let init_player name start_poke = {
  nickname = name;
  starter = start_poke;
  location = (0., 0.);
  poke_list = [start_poke];
  bag = empty_bag;
  balance = 0;
}

let check_pc player = failwith "unimplemented"

let catch_poke player poke = failwith "unimplemented"
