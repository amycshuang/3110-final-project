open State
open Gui

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game player =
  let input = get_key () in
  let p = move_map player (map_key input) in
  render trying p;
  play_game p



(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  (* ANSITerminal.(print_string [red]
                  "\n\nHello trainer, welcome to the PoKaml!\n");
     print_endline "What should I call you?\n";
     print_string  "> ";
     match read_line () with
     | exception End_of_file -> ()
     | file_name -> play_game file_name *)
  play_game test_player


(* Execute the game engine. *)
let () = main ()