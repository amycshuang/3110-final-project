open Initial
open State
open Command
open Encounter
open Pokemon
open Block
open Menu
open Walking
open Gui

(** [render_st st] renders a new GUI screen based on the state. *)
let render_st (st : State.state) = 
  match st.status with 
  | Walking -> render_walk st
  | Enter building -> render_walk st
  | Win -> ()
  | Menu mst -> render_menu st mst
  | Encounter est -> ()
  | Battling _ -> ()

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game st =
  render_st st;
  let input = get_key () in
  let n_st = 
    match st.status with
    | Walking -> process_walk input st
    | Menu mst -> process_menu input st mst
    | _ -> process_walk input st in
  play_game n_st

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  play_game initialize

(* Execute the game engine. *)
let () = main ()