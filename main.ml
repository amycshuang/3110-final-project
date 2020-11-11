open State
open Command
open Gui

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game player =
  let input = get_key () in
  let p = move_map player (map_key input) in
  render trying p;
  play_game p

let confirm_name_message name = name ^ ". Ah that is a nice name. "
                                ^ "Are you sure you want to be called " ^ name ^ " ?" 
let reprompt_name_message = "Okay, then what should I call you?"

let error_message = "Sorry, that is not a valid input. Please try again."

let clarkson_message = 
  "Welcome to the world of pokémon!" ^
  "My name is Clarkson! People call me the pokémon Prof!" ^ 
  "This world is inhabited by creatures called pokémon!" ^ 
  "Your very own Pokémon legend is about to unfold!" ^
  "A world of dreams and adventures with Pokémon awaits! Let's go!"

let prompt_choose_start_message = "Now, "

let start_game name = failwith "unimplemented"

(* 
let rec init_player_starter name msg () = 
  ANSITerminal.erase Screen;
  print_endline msg;
  print_string ">";
  let name = read_line () in 
  print_endline (confirm_name_message name);
  try 
    match parse (read_line ()) parse_yn with 
    | Yes -> init_player_starter
    | No ->  init_player_name reprompt_name_message ()
    | _ -> init_player_name error_message ()
  with 
  | InvalidCommand m -> 
    print_endline error_message;
    init_player_name error_message () *)

(* let rec init_player_name msg () = 
   ANSITerminal.erase Screen;
   print_endline msg;
   print_string ">";
   let name = read_line () in 
   print_endline (confirm_name_message name);
   try 
    match parse (read_line ()) parse_yn with 
    | Yes -> init_player_starter clarkson_message
    | No ->  init_player_name reprompt_name_message ()
    | _ -> init_player_name error_message ()
   with 
   | InvalidCommand m -> 
    print_endline error_message;
    init_player_name error_message () *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let welcome = "\n\nHello trainer, welcome to the PoKaml!\n" ^
                "What should I call you?\n" in 
  init_player_name welcome ()


(* ANSITerminal.(print_string [red]
                "\n\nHello trainer, welcome to the PoKaml!\n");
   print_endline "What should I call you?\n";
   print_string  "> ";
   match read_line () with
   | exception End_of_file -> ()
   | file_name -> play_game file_name *)
(* Must take in a map name *)
(* play_game test_player *)

(* Execute the game engine. *)
let () = main ()
