open Printf

(** [block_error_message] is a string that tells the player that their 
    input was an invalid block. *)
let block_error_message = "Sorry, that is not a valid block type. Please enter a valid block.\n"

(** [block_input_helper msg] pattern matches shorthand inputs and makes 
    sure that the input is a valid block type. *)
let rec block_input_helper msg = 
  match read_line () with 
  | "g" | "gg" | "grass" -> "\"" ^ "grass" ^ "\""
  | "w" | "water" -> "\"" ^ "water" ^ "\""
  | "r" | "road" -> "\"" ^ "road" ^ "\""
  | "t" | "tall grass" -> "\"" ^ "tall grass" ^ "\""
  | "h" | "house" -> "\"" ^ "house" ^ "\""
  | "c" | "pokecenter" -> "\"" ^ "pokecenter" ^ "\""
  | "gym" -> "\"" ^ "gym" ^ "\""
  | "n" | "null" -> "\"" ^ "null" ^ "\""
  | "gr" | "gym road" -> "\"" ^ "gym road" ^ "\""
  | "b" | "brown gym floor" -> "\"" ^ "brown gym floor" ^ "\""
  | "e" | "exit" -> "\"" ^ "exit" ^ "\""
  | "grey" | "grey gym floor" -> "\"" ^ "grey gym floor" ^ "\""
  | "trainer" -> "\"" ^ "trainer" ^ "\""
  | "clarkson spot" -> "\"" ^ "clarkson spot" ^ "\""
  | _ -> 
    ANSITerminal.(print_string [cyan] msg); 
    block_input_helper msg

(** [block_type_input file_name w h ()] takes in [w] * [h] imputs to account 
    for all the blocks needed to complete the map. *)
let rec block_type_input file_name w h () =
  let oc = open_out_gen [Open_append] 0o666 file_name in
  print_string "\n";
  print_string "Enter the block types until this ends\n
  | 'g' for grass | 'w' for water | 'r' for road | 't' for tall grass 
  | 'h' for house | 'c' for pokecenter | 'gym' for gym \n";
  for i=0 to (w * h - 1) do 
    print_string ("\n" ^ (string_of_int (i+1)) ^ ". ");
    let block_type = block_input_helper block_error_message in
    let json = "{\"type\": " ^ block_type ^ "}," in
    fprintf oc "%s\n" json;
  done;
  close_out oc

(** [map_input map_file_name] retreives user input to record map's width, 
    height, and block types in the json file [map_file_name]. *)
let map_input map_file_name = 
  print_string "Enter this map's width\n";
  let map_width = read_int () in
  print_string "\n";

  print_string "Enter this map's height\n";
  let map_height = read_int () in
  print_string "\n";

  let json = "{\"width\": " ^ string_of_int map_width ^ 
             ", \"height\": " ^ string_of_int map_height ^ 
             ", \"blocks\": [ " in

  let oc = open_out_gen [Open_append] 0o666 map_file_name in 
  fprintf oc "%s\n" json;
  close_out oc;

  let blocks = block_type_input map_file_name map_width map_height () in
  print_string "\n";

  let block_json = read_line blocks ^ "]" ^ "}" in
  let oc = open_out_gen [Open_append] 0o666 map_file_name in
  fprintf oc "%s\n" block_json;
  let _ = close_out oc in 
  ()

(** [main ()] prompts for the map json builder and starts it. *)
let rec main () =
  ANSITerminal.(print_string [cyan]
                  "Hi there! To add a map json, please enter the name of the file where this map's json will be stored.\n"); 
  let map_json_file_name = read_line () in
  print_string "\n"; 
  let _ = map_input map_json_file_name in
  () 

(* Execute the map json builder. *)
let () = main ()