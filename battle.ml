open State
open Pokemon
open Player
(** The type of selection on the encounter screen *)
type selection = Up | Left | Right | Down | Select of action

(* 2 pokemon, 4 moves *)

(** [encount ch] is the correpsonding selection action to a key input. *)
let battle_key ch =
  match ch with
  | 'w' -> Some Up
  | 'a' -> Some Left
  | 's' -> Some Down
  | 'd' -> Some Right
  | 'e' -> Some Select button 
  | _ -> None

let choose_attack pkm1 = 
  let moves = pkm1.mvs in 
  display_moves in 
let chosen_move = process+input in 
let newpkm = battledamage pkm1 cosen move

let check_battle st : bool = failwith "unimplemented"

let battle (st : battle_state) = 
  if check_batlte (u can still battle) then 
    if st.p_turn 
    then let p_move = choose_attack st.player.pkm1 
      in battle {st with player.poe = p_move, p_turn = false} 
    else let opp_move = opponent_move st.opponent in 
  else {st }
