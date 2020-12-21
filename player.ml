open Pokemon

(** The type of a Player's nickname. *)
type nickname = string

(** The type of a badge the Player can earn. *)
type badge = string

type item = Potion | Pokeball

(** The type of a player's bag. *)
type bag = {
  badge_case : badge list;
  inventory : (item * int) list;
}

(** The type of values representing a player. *)
type player = {
  nickname : nickname;
  location : int * int;
  poke_list : Pokemon.pokemon list;
  bag : bag;
  balance : int;
}

(** [empty_bag] is a bag with nothing in pc_box, badge_case or inventory. *)
let empty_bag = {
  badge_case = [];
  inventory = [(Potion, 8); (Pokeball, 8)];
}

let init_player name start_poke loc = {
  nickname = name;
  location = loc;
  poke_list = [start_poke];
  bag = empty_bag;
  balance = 500;
}

let catch_poke player poke =
  let new_poke_list = player.poke_list @ [poke] in
  let updated_player =
    {
      nickname = player.nickname;
      location = player.location;
      poke_list = new_poke_list;
      bag = player.bag;
      balance = player.balance
    } in
  updated_player