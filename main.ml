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
  | Walking | WalkingGym | EnterGym | ExitGym -> render_walk st
  | TrainerTalk trainer -> render_trainertalk trainer st 
  | TrainerOver -> render_trainerover st 
  | PokeCenter -> render_pokecenter st
  | CannotBattle -> render_walk st 
  | AlreadyBattled -> render_walk st 
  | Menu mst -> render_menu st mst
  | Win -> render_win st 

let rec play_game st : unit =
  render_st st;
  check_status st;
  let input = get_key () in
  let n_st = 
    match st.status with
    | Walking | WalkingGym | EnterGym | ExitGym -> process_walk input st
    | TrainerTalk trainer -> process_gym input st 
    | AlreadyBattled -> process_walk input st 
    | CannotBattle -> process_walk input st 
    | TrainerOver -> process_gym input st
    | PokeCenter -> process_pokecenter input st  
    | Menu mst -> process_menu input st mst
    | Win -> st in
  play_game n_st
and check_status st : unit = 
  match st.status with
  | Menu mst -> begin
      match mst.status with 
      | Attack _ -> let def_status = {mst with hover = 0; status = Default} in
        play_game {st with status = Menu def_status}                            
      | _ -> ()
    end
  | _ -> ()


(** TODO: look at this code *)
(** [play_game f] starts the adventure in file [f]. *)
(* let rec play_game st =
   render_st st;
   let input = get_key () in
   let n_st = 
    match st.status with 
    | Walking -> process_walk input st
    | WalkingGym -> process_walk input {st with  
          map = Block.json_to_map "map_jsons/gym_map.json"}
    | PokeCenter -> process_pokecenter input st  
    (* | Gym -> process_walk input {st with status = WalkingGym; 
          map = Block.json_to_map "map_jsons/gym_map.json"} *)
    | Menu mst -> process_menu input st mst
    | Win -> failwith "TODO" in
   play_game n_st *)

let main () = play_game initialize

(* Execute the game engine. *)
let () = main ()