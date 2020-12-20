open State
open Pokemon
open Player
(** The type of selection on the encounter screen *)
type selection = Up | Left | Right | Down

(* 2 pokemon, 4 moves *)

(** [encount ch] is the correpsonding selection action to a key input. *)
let battle_key ch =
  match ch with
  | 'w' -> Some Up
  | 'a' -> Some Left
  | 's' -> Some Down
  | 'd' -> Some Right
  | _ -> None
(* 
let choose_attack pkm1 = 
  let moves = pkm1.mvs in 
  display_moves in 
let chosen_move = process+input in 
let newpkm = battledamage pkm1 chosen move

let check_battle st : bool = failwith "unimplemented"



let process_battle st mst = failwith "TODO"