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
  location : float * float;
  poke_list : Pokemon.t list;
  bag : bag;
  balance : int;
}

type t = player

(** [empty_bag] is a bag with nothing in pc_box, badge_case or inventory. *)
let empty_bag = {
  pc_box = [];
  badge_case = [];
  inventory = [];
}

let init_player name start_poke = {
  nickname = name;
  location = (0., 0.);
  poke_list = [start_poke];
  bag = empty_bag;
  balance = 0;
}

(** [move_poke poke_list acc ct] returns a list of Pokemon that need to be 
    removed from poke_list so that poke_list has a length of 6 or less *)
let rec move_poke poke_list acc =
<<<<<<< HEAD
    match poke_list with
    | [] -> []
    | h::t -> if (List.length poke_list) > 6 then move_poke t (h::acc) else acc

=======
  match poke_list with
  | [] -> []
  | h::t -> if (List.length poke_list) > 6 then move_poke t (h::acc) else acc

(** [check_pc player] ensures the player only has 6 pokemon in the player's 
    party, otherwise, extra pokemon are moved to the PC box. *)
>>>>>>> 622852099b09a25f1aa2d723ec64804535958a6b
let check_pc player =
  let to_move = move_poke player.poke_list [] in 
  let new_pc = to_move @ player.bag.pc_box in
  let new_poke_list = 
<<<<<<< HEAD
  List.filter (fun x -> not (List.mem x to_move)) player.poke_list in
  {
    nickname = player.nickname;
    starter = player.starter;
=======
    List.filter (fun x -> not (List.mem x to_move)) player.poke_list in
  {
    nickname = player.nickname;
>>>>>>> 622852099b09a25f1aa2d723ec64804535958a6b
    location = player.location;
    poke_list = new_poke_list;
    bag = {player.bag with pc_box=new_pc};
    balance = player.balance
  }

let catch_poke player poke =
  let new_poke_list = List.cons poke player.poke_list in
  let updated_player =
    {
      nickname = player.nickname;
<<<<<<< HEAD
      starter = player.starter;
=======
>>>>>>> 622852099b09a25f1aa2d723ec64804535958a6b
      location = player.location;
      poke_list = new_poke_list;
      bag = player.bag;
      balance = player.balance
    } in
<<<<<<< HEAD
  check_pc updated_player
=======
  check_pc updated_player

let get_poke_list player = 
  List.map (fun x -> (get_name x, get_level x)) player.poke_list
>>>>>>> 622852099b09a25f1aa2d723ec64804535958a6b
