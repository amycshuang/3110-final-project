open Initial
open State
open Command
open Encounter
open Pokemon
open Block
open Menu
open Walking
open Gui
open Gym
open Pokecenter

(** [render_st st] renders a new GUI screen based on the state. *)
let render_st (st : State.state) = 
  match st.status with 
  | Walking -> render_walk st
  | WalkingGym -> render_walk st
  (* failwith "unimplemented" *)
  | PokeCenter -> render_pokecenter st
  (* | Gym -> render_walk st  *)
  | Menu mst -> render_menu st mst
  | Win -> ()


(** [play_game f] starts the adventure in file [f]. *)
let rec play_game st : unit =
  render_st st;
  check_menu st;
  let input = get_key () in
  let n_st = 
    match st.status with
    | Walking -> process_walk input st
    | PokeCenter -> process_pokecenter input st  
    | WalkingGym -> process_walk input st
    | Menu mst -> process_menu input st mst
    | Win -> failwith "TODO" in
  render_st n_st;
  play_game n_st
and check_menu st : unit = 
  match st.status with 
  | Menu mst -> begin
      match mst.status with 
      | Attack _ -> let def_status = {mst with status = Default} in
        play_game {st with status = Menu def_status}                            
      | _ -> ()
    end
  | _ -> ()

(** [play_game f] starts the adventure in file [f]. *)
(* let rec play_game st =
   render_st st;
   let input = get_key () in
   let n_st = 
    match st.status with
    | Walking -> process_walk input st
    | WalkingGym -> process_walk input {st with  
                                        map = Block.json_to_map "gym_map.json"}
    | PokeCenter -> process_pokecenter input st  
    (* | Gym -> process_walk input {st with status = WalkingGym; 
                                         map = Block.json_to_map "gym_map.json"} *)
    | Menu mst -> process_menu input st mst
    | Win -> failwith "TODO" in
   play_game n_st *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  play_game initialize

(* Execute the game engine. *)
let () = main ()