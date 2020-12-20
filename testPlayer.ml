(**
   Tests for all functions in player.ml.
*)

open OUnit2
open Player

let init_player_test
    (test_name : string)
    (name : string)
    (start_poke : Pokemon.pokemon)
    (loc : int * int)
    (expected_output) : test =
  name >:: (fun _ -> assert_equal expected_output (init_player name start_poke
                                                     loc))


