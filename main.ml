open Initial
open State
open Command
open Encounter
open Pokemon
open Block
open Walking
open Gui

(** [check_st st] *)
let check_st (st : State.state) = 
  match st.status with 
  | Walking -> render_walk st
  | Battling -> ()
  | Enter building -> render_walk st
  | Win -> ()
  | Encounter b_type ->
    let spawned = spawn_poke b_type in
    match spawned with
    | Some x -> let est = { player = st.player; opponent = x} in 
      render_encounter st est;
    | None -> render_walk st

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game st =
  check_st st;
  let input = get_key () in
  let n_st = 
    match st.status with
    | Walking -> process_walk input st
    | Encounter t -> process_walk input st
    | _ -> process_walk input st in
  play_game n_st

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  play_game initialize

(* Execute the game engine. *)
let () = main ()