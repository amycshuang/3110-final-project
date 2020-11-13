open State
open Gui
open Command
open Pokemon

let confirm_name_message name = name ^ ". Ah, that is a nice name. "
                                ^ "Are you sure you want to be called " 
                                ^ name ^ "? (yes/no)" 

let reprompt_name_message = "Okay, then what should I call you?\n"

let error_message = "Sorry, that is not a valid input. Please try again.\n"

let clarkson_message = 
  "Welcome to the world of pokémon! \n" ^
  "My name is Clarkson. People call me the pokémon Prof! \n" ^ 
  "This world is inhabited by creatures called pokémon. \n" ^ 
  "Your very own Pokémon legend is about to unfold! \n" ^
  "A world of dreams and adventures with Pokémon awaits. Let's go! \n\n" ^ 
  "Now, which Pokemon do you want?\n\n" 

let bulbasaur = "Bulbasaur"
let charmander = "Charmander"       
let squirtle = "Squirtle"

let reprompt_starter_message name = 
  "Sorry " ^ name ^ "!" ^ 
  "That is not one of the starter pokemon we have available " ^ 
  "Choose again!"

exception InvalidStarterJson

let starter_poke = Pokemon.poke_list_from_json (Yojson.Basic.from_file "starter_pokemon.json")

let rec find_starter s = 
  let checking = List.map (fun x -> x.name) starter_poke in
  print_endline (List.nth checking 2);
  match List.filter (fun x -> (String.lowercase_ascii x.name) = s) starter_poke with 
  | [] -> raise InvalidStarterJson
  | h :: t -> h 

(** [init_player_name name msg] selects the player's starter. *)
let rec init_player_starter name msg = 
  ANSITerminal.(print_string [cyan] msg);
  print_endline "";
  print_endline "";
  ANSITerminal.(print_string [green] bulbasaur);
  print_endline "";
  ANSITerminal.(print_string [red] charmander);
  print_endline "";
  ANSITerminal.(print_string [blue] squirtle);
  print_endline "";
  print_endline "";
  print_string "> ";
  try 
    match parse (read_line ()) parse_starter with 
    | SPokemon ("charmander" as c) -> init_state name (find_starter c)
    | SPokemon ("bulbasaur" as b) ->  init_state name (find_starter b)
    | SPokemon ("squirtle" as s) -> init_state name (find_starter s) 
    | SPokemon ("pikachu" as p) -> init_state name (find_starter p)
    | _ -> init_player_starter name (reprompt_starter_message name) 
  with 
  | InvalidPokemon m -> init_player_starter name (reprompt_starter_message name) 

(** [init_player_name] *)
(* let rec init_map_msg =
   let choose_map_msg = "Welcome to PoKaml! Which map would you like to play 
   today?" in
   ANSITerminal.(print_string [white] choose_map_msg);
   print_endline "";
   print_string "> "; *)


(** [init_player_name msg] selects the player's name. *)
let rec init_player_name msg = 
  ANSITerminal.(print_string [cyan] msg);
  print_string "> ";
  let name = read_line () in 
  ANSITerminal.(print_string [cyan] (confirm_name_message name));
  print_endline "";
  print_string "> ";
  try 
    match parse (read_line ()) parse_yn with 
    | Yes -> init_player_starter name clarkson_message
    | No ->  init_player_name reprompt_name_message 
    | _ -> init_player_name error_message 
  with 
  | InvalidCommand m -> 
    init_player_name error_message 


let initialize =
  let msg = "\n\nHello trainer, welcome to the PoKaml!\n" ^
            "What should I call you?\n" in
  init_player_name msg