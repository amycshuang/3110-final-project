open State
open Gui

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game st =
  let input = get_key () in
  let st = process_input input st in
  render st;
  play_game st


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  (* ANSITerminal.(print_string [red]
                  "\n\nHello trainer, welcome to the PoKaml!\n");
     print_endline "What should I call you?\n";
     print_string  "> ";
     match read_line () with
     | exception End_of_file -> ()
     | file_name -> play_game file_name *)
  (* Must take in a map name *)
  play_game testing_state

(* Execute the game engine. *)
let () = main ()