open Player
open State
open Pokemon
open Gui
open Menu

type gym_action =  StartBattle | Default

let gym_key ch = 
  match ch with 
  | 'b' -> StartBattle 
  | _ -> Default

(** make sure you battle the trainers in order for example if trainer is the
    first index in the array then thats fine but if trainer is the second index 
    in the array then you should not be able to battle them 
    with panel text ? *)

let trainer_battle st =   
  match List.filter (fun (t : Trainer.trainer) -> 
      (t.x, t.y) = st.player.location) st.trainers with 
  | [] -> failwith "impossible"
  | h :: t -> h 

let process_gym input st = 
  if List.hd (List.rev st.trainers) = trainer_battle st then
    let mst =                 
      {status = Default;
       player = st.player; 
       opponent = (List.hd (List.rev st.trainers)).poke_list; 
       hover = 0; 
       select = None;
       is_trainer = true
      } in 
    {st with status = Menu mst}
  else {st with panel_txt = "You must battle in order!"; status = WalkingGym}


(* match gst.status with 
   | CannotBattle -> 
   {st with panel_txt = "You must battle in order!"; status = WalkingGym}
   | Begin -> begin 
    match gym_key input with 
    | StartBattle -> {st with status = GymBattle {gst with status = Battling}}
    | Default -> st end 
   | Battling -> begin 
    let n_st = process_menu input st gst.battle in 
    match n_st.status with 
    | Walking -> {st with status = GymBattle {gst with status = Over}}
    | PokeCenter -> n_st
    | Menu mst -> begin 
        match mst.status with 
        | Attack _ -> 
          let mst' = {mst with select = None; status = Default} in 
          {st with status = GymBattle {gst with battle = mst'}}
        | _ -> {st with status = GymBattle {gst with battle = mst}}
      end 
    | _ -> failwith "impossible"
   end 
   | Over -> 
   (* let new_trainers - *)
   (** remove the trainer from the state's trainer list *)
   {st with status = WalkingGym} *)