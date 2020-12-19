open Player
open State
open Pokemon
open Gui


let process_gym st = 
  {st with status = WalkingGym; map = Block.json_to_map "gym_map.json"}

