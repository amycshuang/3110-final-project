open State
open Player
open Pokemon

type action = Fight | PokeList | Bag | Run

(** The type of selection on the encounter screen *)
type selection = Up | Left | Right | Down

(** [encount ch] is the correpsonding selection action to a key input. *)
let encount_key ch =
  match ch with
  | 'w' -> Some Up
  | 'a' -> Some Left
  | 's' -> Some Down
  | 'd' -> Some Right
  | _ -> None

(* TODO : process encounter*)
let process_encounter input (st : state) = st
