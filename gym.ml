open Player
open State
open Pokemon
open Gui

(** [gym_entrance_loc map] is the location of the gym entrance on [map]. *)
(* let gym_entrance_loc map = 
   let ncol = Array.length map.(0) in 
   let nrow = Array.length map in 
   let loc = ref (0, 0) in 
   for row = 0 to (nrow - 1) do 
    for col = 0 to (ncol - 1) do 
      if map.(row).(col) = Block.Exit then 
        loc := (col, row)
    done; 
   done;
   !loc

   (** [gym_loc map] is the location of the gym on [map]. *)
   let gym_loc map = 
   let ncol = Array.length map.(0) in 
   let nrow = Array.length map in 
   let loc = ref (0, 0) in 
   for row = 0 to (nrow - 1) do 
    for col = 0 to (ncol - 1) do 
      if map.(row).(col) = Block.Gym then 
        loc := (col, row)
    done; 
   done;
   !loc

   let process_gym st = 
   if st.status = EnterGym then 
    let loc = gym_entrance_loc st.maps.(1) in 
    let mv_player = {st.player with location = loc} in 
    {st with player = mv_player; status = WalkingGym}
   else 
    let loc = gym_loc st.maps.(0) in 
    let mv_player = {st.player with location = loc} in 
    {st with player = mv_player; status = Walking} *)