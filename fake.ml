open Printf
let grass_file = "grass_pokemon.json"
let water_file = "water_pokemon.json"

(** string_input_helper is the string that the user inputs in the terminal. 
    An important thing to remember is that if you press enter by mistake, nothing 
    will be taken in, and the function will default to an empty string. This is
    intentional. If this was a mistake and you want to change this, you have to 
    do so manually in card_data.json *)
let string_input_helper () =  
  match read_line () with
  | exception End_of_file -> print_endline "nothing given, defaulting to empty string"; ""
  | s -> "\"" ^ s ^ "\""

(** int_input_helper is the integer that the user inputs in the terminal. 
    An important thing to remember is that if you press enter by mistake, nothing 
    will be taken in, and the function will default to 0. This is
    intentional. If this was a mistake and you want to change this, you have to 
    do so manually in card_data.json *)
let int_input_helper () =
  match read_int_opt () with
  | None -> print_endline "setting to 0"; 0
  | Some s -> s

let list_input_helper () = 
  match read_line () with
  | exception End_of_file -> print_endline "setting to []"; "[]"
  | s -> "[" ^ s ^ "]"

(** continue_check_helper [f] will call the function f if the user input is 
    'y' or 'Y', or it will simply return unit (thus exiting) *)
let continue_check_helper (f: unit -> unit) = 
  match read_line () with
  | "y" | "Y" -> f ()
  | _ -> ()

(* Get user input to record a pokemon's move set *)
let rec move_set_input () =
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

  let oc = open_out_gen [Open_append] 0o666 grass_file in (* The octal specifies file permissions *)
  fprintf oc "%s\n" json;
  close_out oc;
  print_string("Enter another move? y/n \n");
  let _ = continue_check_helper move_set_input in
  ()

let rec pokemon_info_input file_name () = 
  print_string "Enter the name of this pokemon\n";
  let pokemon_name = string_input_helper () in
  print_string "\n";

  print_string "Enter the type of this pokemon\n";
  let pokemon_type = string_input_helper () in
  print_string "\n";

  print_string "Enter the level of this pokemon\n";
  let pokemon_level = int_input_helper () in
  print_string "\n";

  print_string "Enter the hp of this pokemon\n";
  let pokemon_hp = int_input_helper () in
  print_string "\n";

  print_string "Enter the attack of this pokemon\n";
  let pokemon_attack = int_input_helper () in
  print_string "\n";

  print_string "Enter the defense of this pokemon\n";
  let pokemon_defense = int_input_helper () in
  print_string "\n";

  print_string "Enter the current experience of this pokemon\n";
  let pokemon_curr_exp = int_input_helper () in
  print_string "\n";

  print_string "Enter the experience needed to level up this pokemon\n";
  let pokemon_level_up_exp = int_input_helper () in
  print_string "\n";

  print_string "Enter the experience needed to level up this pokemon\n";
  let pokemon_moves = move_set_input () in
  print_string "\n";

  let json = "{\"name\": " ^ pokemon_name ^ 
             ", \"poke_type\": " ^ pokemon_type ^ 
             ", \"stats\": { " ^
             "\"level\": " ^ string_of_int pokemon_level ^ 
             ", \"hp\": " ^ string_of_int pokemon_hp ^ 
             ", \"attack\": " ^ string_of_int pokemon_attack ^ 
             ", \"defense\": " ^ string_of_int pokemon_defense ^
             ", \"curr_exp\": " ^ string_of_int pokemon_curr_exp ^ 
             ", \"level_up_exp\": " ^ string_of_int pokemon_level_up_exp ^
             "}, " ^
             "\"caught\": " ^ "false" ^
             ", \"move_set\": " ^ list_input_helper pokemon_moves ^
             "}," in

  let oc = open_out_gen [Open_append] 0o666 file_name in (* The octal specifies file permissions *)
  fprintf oc "%s\n" json;
  close_out oc;
  print_string("Enter another pokemon that appears on this type of block? y/n \n");
  let _ = continue_check_helper (pokemon_info_input file_name) in
  ()

(* Get user input to record a pokemon found in water block *)
let rec water_block_input = 
  pokemon_info_input water_file

(* Get user input to record a pokemon that appears on grass *)
let rec grass_block_input =
  pokemon_info_input grass_file

let rec main () =
  ANSITerminal.(print_string [cyan]
                  "Hello! To add a JSON type, enter a block type. 
                  Please enter all pokemon that appear on a certain block type together, i.e, all pokemon that 
                  can be found in water first and then all pokemon that can be found in grass after.\n");
  ANSITerminal.(print_string [cyan]
                  "The block types are:
                    water and grass\n");
  let _ = match read_line () with
    | "water" -> print_string "\n"; water_block_input ();
    | "grass" -> print_string "\n"; grass_block_input ();
    | _ -> ANSITerminal.(print_string [red] "Invalid block type, try again\n"); 
      main (); in
  print_string "Do you want to enter another block type?";
  let _ = continue_check_helper main in
  () 

let () = main ()