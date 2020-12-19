open Pokemon

type nickname = string
type badge = string
type item = Potion | Pokeball

(** The type of a player's bag. *)
type bag = {
  (* pc_box : Pokemon.pokemon list; *)
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
  (* pc_box = []; *)
  badge_case = [];
  inventory = [(Potion, 5); (Pokeball, 5)];
}

let init_player name start_poke loc = {
  nickname = name;
  location = loc;
  (* poke_list = [lower_hp start_poke]; *)
  poke_list = [start_poke;];
  bag = empty_bag;
  balance = 500;
}

(** [move_poke poke_list acc ct] returns a list of Pokemon that need to be 
    m  removed from poke_list so that poke_list has a length of 6 or less *)
let rec move_poke poke_list acc =
  match poke_list with
  | [] -> []
  | h::t -> if (List.length poke_list) > 6 then move_poke t (h::acc) else acc

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
