open Printf

(** string_input_helper is the string that the user inputs in the terminal. 
    An important thing to remember is that if you press enter by mistake, 
    nothing will be taken in, and the function will default to an empty string. *)
let string_input_helper () =  
  match read_line () with
  | exception End_of_file -> print_endline "nothing given, default to empty string"; ""
  | s -> "\"" ^ s ^ "\""

(** int_input_helper is the integer that the user inputs in the terminal. 
    An important thing to remember is that if you press enter by mistake, 
    nothing will be taken in, and the function will default to 0. *)
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

exception InvalidBlock

let block_error_message = "Sorry, that is not a valid block type."

let rec block_input_helper msg = 
  try
    match read_line () with 
    | "g" | "gg" -> "\"" ^ "grass" ^ "\""
    | "w" -> "\"" ^ "water" ^ "\""
    | "r" -> "\"" ^ "road" ^ "\""
    | "t" -> "\"" ^ "tall grass" ^ "\""
    | "h" -> "\"" ^ "house" ^ "\""
    | "c" -> "\"" ^ "pokecenter" ^ "\""
    | "gym" -> "\"" ^ "gym" ^ "\""
    | "n" -> "\"" ^ "null" ^ "\""
    | "b" -> "\"" ^ "brown gym floor" ^ "\""
    | "grey" -> "\"" ^ "grey gym floor" ^ "\""
    | "trainer" -> "\"" ^ "trainer" ^ "\""
    | "clarkson" -> "\"" ^ "clarkson spot" ^ "\""
    | _ -> block_input_helper msg
  with 
  | InvalidBlock -> block_input_helper msg

(* Gets user input to record a pokemon's move set *)
let rec block_type_input file_name w h () =
  print_string "\n";
  print_string "Enter the block types until this ends\n
  | 'g' for grass | 'w' for water | 'r' for road | 't' for tall grass 
  | 'h' for house | 'c' for pokecenter | 'gym' for gym \n";

  let oc = open_out_gen [Open_append] 0o666 file_name in
  for i=0 to (w * h - 1) do 
    let block_type = block_input_helper block_error_message in
    print_string ("\n" ^ (string_of_int (i+2)) ^ ". ");

    let json = "{\"type\": " ^ block_type ^ "}," in

    fprintf oc "%s\n" json;
  done;
  close_out oc

let map_input map_file_name = 
  print_string "Enter this map's width\n";
  let map_width = int_input_helper () in
  print_string "\n";

  print_string "Enter this map's height\n";
  let map_height = int_input_helper () in
  print_string "\n";

  let json = "{\"width\": " ^ string_of_int map_width ^ 
             ", \"height\": " ^ string_of_int map_height ^ 
             ", \"blocks\": [ " in

  let oc = open_out_gen [Open_append] 0o666 map_file_name in 
  fprintf oc "%s\n" json;
  close_out oc;

  print_string "Now you can enter a list of this map's blocks \n";
  let blocks = block_type_input map_file_name map_width map_height () in
  print_string "\n";

  let block_json = read_line blocks ^ "]" ^ "}" in
  let oc = open_out_gen [Open_append] 0o666 map_file_name in
  fprintf oc "%s\n" block_json;
  close_out oc;

  print_string("Enter another map? y/n \n");
  let _ = continue_check_helper 
      (block_type_input map_file_name map_width map_height) in
  ()

let rec main () =
  ANSITerminal.(print_string [cyan]
                  "Hi there! To add a map json, please enter the name of the file where this map's json will be stored.\n"); 
  let _ = match read_line () with
    | x -> print_string "\n"; map_input x;
      main (); in 
  print_string "all done?\n";
  let _ = continue_check_helper main in
  () 

let () = main ()