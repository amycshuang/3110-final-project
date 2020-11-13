open State
open Player
open Pokemon

type selection = Up | Left | Right | Down

let encount_key ch =
  match ch with
  | 'w' -> Some Up
  | 'a' -> Some Left
  | 's' -> Some Down
  | 'd' -> Some Right
  | _ -> None

let process_encounter input (st : state) = st
(* let action = encount_key input in 
   match action with 
   | Move dir -> begin 
    let mv_st =  {st with player=(move_map st.player dir st.map)} in 
    {mv_st with status = (update_status (player_block mv_st.player mv_st.map))}
   end 
   | Display x -> {st with panel_txt=(display st x)} *)
