open State
open Gui
open Command
open Pokemon

(** [choose_map_message name] is a string that prompts the player to choose a 
    map *)
let choose_map_message name = 
  "Oh, I almost forgot to ask! Where are you from " ^ name ^ "?"

(** [confirm_name_message name] is a string that prompts the player to confirm
    their chosen name *)
let confirm_name_message name = name ^ ". Ah, that is a nice name. "
                                ^ "Are you sure you want to be called " 
                                ^ name ^ "? (yes/no)" 

(** [reprompt_name_message] is a string that reprompts the player to choose a 
    different name. *)
let reprompt_name_message = "Okay, then what should I call you?\n"

(** [error_message] is a string that tells the player that their input was 
    invalid. *)
let error_message = "Sorry, that is not a valid input. Please try again.\n"

(** [clarkson_message] is a string that represents the opening message by
    Professor Clarkson and prompts the player to choose a starter pokemon. *)
let clarkson_message = 
  "Oh wow! I've heard it's beautiful there! \n" ^ 
  "Now...\n" ^
  "Welcome to the world of pokémon! \n" ^
  "My name is Clarkson. People call me the pokémon Prof! \n" ^ 
  "This world is inhabited by creatures called pokémon. \n" ^ 
  "Your very own Pokémon legend is about to unfold! \n" ^
  "A world of dreams and adventures with Pokémon awaits. Let's go! \n\n" ^ 
  "Now, which Pokemon do you want?\n\n" 

(** [bulbasaur] is the string representing the name of the pokemon Bulbasaur *)
let bulbasaur = "Bulbasaur"

(** [charmander] is the string representing the name of the pokemon 
    Charmander *)
let charmander = "Charmander"  

(** [squirtle] is the string representing the name of the pokemon 
    Squirtle *)
let squirtle = "Squirtle"

(** [regions] is a string representing the possible maps/regions to choose. *)
let regions = "Kanto | Johto | Hoenn | Sinnoh | Unova | Kalos | Alola | Galar"

(** [reprompt_region_message] is a string that reprompts the player for a 
    valid region. *)
let reprompt_region_message = "Oh, where is that again? " ^
                              "Could you please tell me again?"

(** [reprompt_starter_message] is a string that reprompts the player for a 
    valid starter pokemon. *)
let reprompt_starter_message name = 
  "Sorry " ^ name ^ "! " ^ 
  "That is not one of the starter pokemon we have available. " ^ 
  "Choose again!"

(** Raised when the Player enters an invalid pokemon for the desired starter 
    pokemon. *)
exception InvalidStarterJson

(** [starter_poke] is the list of starter pokemon *)
let starter_poke = 
  Pokemon.poke_list_from_json (Yojson.Basic.from_file "starter_pokemon.json")

(** [find_starter s] finds the correct starter pokemon with the name [s] in the 
    starter pokemon list *)
let rec find_starter s = 
  let filtered =
    List.filter (fun x -> (String.lowercase_ascii x.name) = s) starter_poke in
  match filtered with 
  | [] -> raise InvalidStarterJson
  | h :: t -> h 

(** [init_player_name name msg] selects the player's starter. *)
let rec init_player_starter msg name map = 
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
    | SPokemon ("charmander" as c) -> init_state name (find_starter c) map
    | SPokemon ("bulbasaur" as b) ->  init_state name (find_starter b) map
    | SPokemon ("squirtle" as s) -> init_state name (find_starter s) map
    | SPokemon ("pikachu" as p) -> init_state name (find_starter p) map
    | _ -> init_player_starter (reprompt_starter_message name) name map
  with 
  | InvalidPokemon m | InvalidCommand m ->
    init_player_starter (reprompt_starter_message name) name map

(** [init_map_msg] initializes the map *)
let rec init_map_msg name msg =
  ANSITerminal.(print_string [cyan] msg);
  print_endline "";
  print_endline "";
  ANSITerminal.(print_string [yellow] regions);
  print_endline "";
  print_string "> ";
  try 
    match parse (read_line ()) parse_region with 
    | Map j -> let map = Block.json_to_map j in
      init_player_starter clarkson_message name map
    | _ -> init_map_msg name reprompt_region_message
  with
  | InvalidRegion -> init_map_msg name reprompt_region_message

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
    | Yes -> init_map_msg name (choose_map_message name)
    | No ->  init_player_name reprompt_name_message 
    | _ -> init_player_name error_message 
  with 
  | InvalidCommand m -> 
    init_player_name error_message 

(** [initialize] is the inital game state *)
let initialize =
  let msg = "\n\nHello trainer, welcome to the PoKaml!\n" ^
            "What should I call you?\n" in
  init_player_name msg