open State
open Command
open Pokemon
open Gui

let confirm_name_message name = name ^ ". Ah that is a nice name. "
                                ^ "Are you sure you want to be called " ^ name ^ "?" 
let reprompt_name_message = "Okay, then what should I call you?"

let error_message = "Sorry, that is not a valid input. Please try again."

let clarkson_message = 
  "Welcome to the world of pokémon! \n" ^
  "My name is Clarkson! People call me the pokémon Prof! \n" ^ 
  "This world is inhabited by creatures called pokémon! \n" ^ 
  "Your very own Pokémon legend is about to unfold! \n" ^
  "A world of dreams and adventures with Pokémon awaits! Let's go! \n\n" ^ 
  "Now, which Pokemon do you want?\n\n" 

let bulbasaur = "Bulbasaur"
let charmander = "Charmander"       
let squirtle = "Squirtle"

let reprompt_starter_message name = 
  "Sorry " ^ name ^ "!" ^ 
  "That is not one of the starter pokemon we have available " ^ 
  "Choose again!"

let start_game_message name = 
  "Good choice " ^ name ^ "! " ^ 
  "Good luck on your adventure! Peace, love, and 3110" 

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game st =
  let input = get_key () in
  let st = process_input input st in
  render st;
  play_game st

let start_game name pkm = 
  ANSITerminal.(print_string [cyan] (start_game_message name));
  print_endline "";
  let state = init_state name pkm in 
  render state;
  play_game state 

let rec init_player_starter name msg = 
  ANSITerminal.(print_string [cyan] msg);
  print_endline "";
  print_string "> ";
  try 
    match parse (read_line ()) parse_starter with 
    | SPokemon ("charmander" as c) -> start_game name c
    | SPokemon ("bulbasaur" as b) ->  start_game name b 
    | SPokemon ("squirtle" as s) -> start_game name s 
    | SPokemon ("pikachu" as p) -> start_game name p
    | _ -> init_player_starter name (reprompt_starter_message name) 
  with 
  | InvalidPokemon m -> init_player_starter name (reprompt_starter_message name) 

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

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let welcome = "\n\nHello trainer, welcome to the PoKaml!\n" ^
                "What should I call you?\n" in 
  init_player_name welcome  


(* Execute the game engine. *)
let () = main ()
