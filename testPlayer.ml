(**
   Tests for all functions in player.ml.
*)

open OUnit2
open Player

let init_player_test
    (name: string)
    (player: Player.t)
    (expected_output) : test =
  name >:: (fun _ -> assert_equal expected_output (player.poke_list))


