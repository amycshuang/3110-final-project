open Player
open Pokemon

type t = {
  player: Player.t; 
}

type status = Champion | InProgress

(** [get_player st] is the current data of the player. *)
let get_player = Player.t

(** [update_player_bag st] is the player after their bag has been updated. *)
let update_player_bag b = Player.t.bag = b

let string_of_type = function 
  | Bug -> "Bug"
  | Dark -> "Dark"
  | Dragon -> "Dragon"
  | Electric -> "Electric"
  | Fighting -> "Fighting"
  | Fire -> "Fire"
  | Flying -> "Flying"
  | Ghost -> "Ghost"
  | Grass -> "Grass"
  | Ice -> "Ice"
  | Normal -> "Normal"
  | Poison -> "Poison"
  | Psychic -> "Psychic"
  | Rock -> "Rock"
  | Steel -> "Steel"
  | Water -> "Water"
  | _ -> raise (InvalidPokemon "this pokemon does not have a type")

(** [string_of_tuple] is the string representation of tuple [t] *)
let string_of_tuple t = 
  "(" ^ string_of_float t.fst ^ ", " ^ string_of_float t.snd ^ ")"

(** [string_of_move] is the string representation of the move set of 
    pokemon [poke] *)
let rec string_of_move poke = 
  match poke with 
  | [] -> 
  | h -> "(" ^ h.move_name ^ ", " ^ h.move_type ^ ")"
  | h :: t -> "(" ^ h.move_name ^ ", " ^ h.move_type ^ "), " ^ string_of_move t

(** [print_pokemon_stats p_stats] formats and prints a 
    pokemon's stats [p_stats] *)
let print_pokemon_stats p_stats = 
  print_string "Level: " ^ (string_of_int p_stats.level);
  print_string ", HP: " ^ (string_of_int p_stats.hp);
  print_string ", Attack: " ^ (string_of_int p_stats.attack);
  print_string ", Defense: " ^ (string_of_int p_stats.defense);
  print_string ", Current Experience: " ^ (string_of_int p_stats.curr_exp);
  print_string ", Level Up Experience: " ^ (string_of_int p_stats.level_up_exp ^ "\n")

(** [print_pokemon poke] prints the information of pokemon [poke] *)
let print_pokemon poke = 
  print_endline ("Name: " ^ poke.name);
  print_endline ("Type: " ^ string_of_type poke.poke_type);
  print_endline ("Stats: {" ^ print_pokemon_stats poke.stats ^ "}");
  print_endline ("Moves: [" ^ string_of_move poke.move_set ^ "]")

(** [string_of_player_data pl] prints the player's current data. *)
let print_player_data p =
  print_endline ("Name: " ^ p.nickname);
  print_endline ("Location: " ^ string_of_tuple p.location);
  print_endline ("Balance: " ^ string_of_int p.balance)

(* pc_box : Pokemon.t list;
   badge_case : badge list;
   inventory : (item * int) list;
   poke_list : Pokemon.t list;
   bag : bag;s

   let rec print_pc_box p = 
   match p.bag.pc_box with
   | [] -> print_endline "________________________"
   | h :: t -> Movable.tank_info h; print_pc_box t *)