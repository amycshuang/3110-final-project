open Initial
open State
open Command
open Encounter
open Pokemon
open Walking
open Gui

(** TODO - call the actual spawning function from Block *)
let fake_spawn_poke = Pokemon.poke_from_json (Yojson.Basic.from_file "pikachu.json")

(** [check_st st] *)
(* let check_st (st : State.state) = 
   match st.status with 
   | Walking -> ()
   | Battling -> ()
   | Encounter b_type -> 
    if b_type = TallGrass then render_encounter st TallGrass fake_spawn_poke fake_spawn_poke
    else render_encounter st Water fake_spawn_poke fake_spawn_poke
   | Enter building -> ()
   | Win -> () *)

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game st =
  render st;
  let input = get_key () in
  (* check_st st; *)
  let n_st = 
    match st.status with 
    | Walking -> process_walk input st
    (* | Encounter t -> process_encounter input st *)
    | _ -> process_walk input st in
  render n_st;
  play_game n_st

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  render initialize;
  play_game initialize

(* Execute the game engine. *)
let () = main ()
