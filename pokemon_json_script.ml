open Printf

(** string_input_helper is the string that the user inputs in the terminal. 
    An important thing to remember is that if you press enter by mistake, nothing 
    will be taken in, and the function will default to an empty string. *)
let string_input_helper () =  
  match read_line () with
  | exception End_of_file -> print_endline "nothing given, defaulting to empty string"; ""
  | s -> "\"" ^ s ^ "\""

(** int_input_helper is the integer that the user inputs in the terminal. 
    An important thing to remember is that if you press enter by mistake, nothing 
    will be taken in, and the function will default to 0. *)
let int_input_helper () =
  match read_int_opt () with
  | None -> print_endline "setting to 0"; 0
  | Some s -> s

(** continue_check_helper [f] will call the function f if the user input is 
    'y' or 'Y', or it will simply return unit (thus exiting) *)
let continue_check_helper (f: unit -> unit) = 
  match read_line () with
  | "y" | "Y" -> f ()
  | _ -> ()

(* Gets user input to record a pokemon's move set *)
let rec move_set_input file_name () =
  print_string "\n";
  print_string "Enter the type of this move\n";
  let poke_move_type = string_input_helper () in
  print_string "\n";

  print_string "Enter the name of this move\n";
  let poke_move_name = string_input_helper () in
  print_string "\n";

  let json = "{\"move_type\": " ^ poke_move_type ^ 
             ", \"move_name\": " ^ poke_move_name ^ 
             "}," in

  let oc = open_out_gen [Open_append] 0o666 file_name in
  fprintf oc "%s\n" json;
  close_out oc;
  print_string("Enter another move? y/n \n");
  let _ = continue_check_helper (move_set_input file_name) in
  ()

(* [pokemon_info_input file_name] formats and prints a pokemon json that appears 
   on a certain block type in its corresponding file [file_name] *)
let rec pokemon_info_input file_name () = 
  print_string "Enter this pokemon's name\n";
  let pokemon_name = string_input_helper () in
  print_string "\n";

  print_string "Enter this pokemon's type (note: please capatilize the first letter)\n";
  let pokemon_type = string_input_helper () in
  print_string "\n";

  print_string "Enter this pokemon's base HP\n";
  let pokemon_base_hp = int_input_helper () in
  print_string "\n";

  print_string "Enter this pokemon's attack\n";
  let pokemon_attack = int_input_helper () in
  print_string "\n";

  print_string "Enter this pokemon's defense\n";
  let pokemon_defense = int_input_helper () in
  print_string "\n";

  let json = "{\"name\": " ^ pokemon_name ^ 
             ", \"poke_type\": " ^ pokemon_type ^ 
             ", \"stats\": { " ^
             "\"level\": " ^ string_of_int ((Random.int 9) + 1) ^ 
             ", \"base_hp\": " ^ string_of_int pokemon_base_hp ^ 
             ", \"hp\": " ^ string_of_int pokemon_base_hp ^ 
             ", \"attack\": " ^ string_of_int pokemon_attack ^ 
             ", \"defense\": " ^ string_of_int pokemon_defense ^
             ", \"curr_exp\": " ^ "0" ^ 
             ", \"level_up_exp\": " ^ "50" ^
             "}, " ^
             "\"caught\": " ^ "false" ^
             ", \"move_set\": [ " in

  let oc = open_out_gen [Open_append] 0o666 file_name in 
  fprintf oc "%s\n" json;
  close_out oc;

  print_string "Now you can enter a list of this pokemon's moves\n";
  let pokemon_moves = move_set_input file_name () in
  print_string "\n";

  let move_json = read_line pokemon_moves ^ "]" ^ "}," in
  let oc = open_out_gen [Open_append] 0o666 file_name in
  fprintf oc "%s\n" move_json;
  close_out oc;

  print_string("Enter another pokemon that appears on this type of block? y/n \n");
  let _ = continue_check_helper (pokemon_info_input file_name) in
  ()

(* Gets user input to record a pokemon found in water block *)
let water_block_input = pokemon_info_input "water_pokemon.json"

(* Gets user input to record a pokemon that appears on grass *)
let grass_block_input = pokemon_info_input "grass_pokemon.json"

(* Gets user input to record one of the starter pokemon *)
let starter_block_input = pokemon_info_input "starter_pokemon.json"

let rec main () =
  ANSITerminal.(print_string [cyan]
                  "Hi there! To add a pokemon json, enter the block type on which this pokemon would spawn.\n");
  ANSITerminal.(print_string [cyan]
                  "The block types are: water and grass\n");
  let _ = match read_line () with
    | "water" -> print_string "\n"; water_block_input ();
    | "grass" -> print_string "\n"; grass_block_input ();
    | "starter" -> print_string "\n"; starter_block_input ();
    | _ -> ANSITerminal.(print_string [red] "Invalid block type, try again\n"); 
      main (); in
  print_string "Do you want to enter another block type?\n";
  let _ = continue_check_helper main in
  () 

let () = main ()