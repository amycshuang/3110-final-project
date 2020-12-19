
(** 
   Tests for all functions in Pokemon.ml
*)
open OUnit2
open Pokemon
open TestVariables

let type_from_string_test (name: string) (s_type: string) 
    (expected_output: poke_type) : test = 
  name >:: (fun _ -> assert_equal expected_output (type_from_string s_type))

let type_from_string_test_exn (name: string) (s_type: string) (ex: exn) : test 
  = name >:: (fun _ -> assert_raises ex (fun () -> type_from_string s_type))

let opponent_move_test (name: string) (pkm: pokemon) 
  : test = name >:: (fun _ -> assert_equal true 
                        (List.mem (opponent_move pkm) pkm.move_set))

let battle_damage_test (name: string) (pkm1: pokemon) (pkm2: pokemon) 
    (move: move) (expected_output : pokemon) : test = 
  name >:: (fun _ -> assert_equal expected_output (battle_damage pkm1 pkm2 move)) 

let level_up_test (name: string) (pkm: pokemon)
    (expected_output: pokemon) : test  =
  name >:: (fun _ -> assert_equal expected_output (level_up pkm))

let increase_exp_test (name: string) (pkm_one: pokemon) (pkm_two: pokemon)
    (expected_output: pokemon) : test = 
  name >:: 
  (fun _ -> assert_equal expected_output (increase_exp pkm_one pkm_two))

let type_from_string_tests = [
  (* testing non-exception *)
  type_from_string_test "type 'Dark' is Dark" "Dark" Dark;
  type_from_string_test "type 'Ice' is Ice" "Ice" Ice;

  (* testing exception *)
  type_from_string_test_exn "type 'ice' is an invalid pokemon type" "ice" 
    (InvalidPokemonType "this pokemon type is not valid");
  type_from_string_test_exn "type 'Aira' is an invalid pokemon type" "Aira"
    (InvalidPokemonType "this pokemon type is not valid");
]

let opponent_move_tests = [
  opponent_move_test "a valid starter pokemon move was obtained"
    test_starter_pkm;
  opponent_move_test "a valid grass pokemon move was obtained"
    test_grass_pkm;
  opponent_move_test "a valid water pokemon move was obtained"
    test_water_pkm;
]

let battle_damage_tests = [
  (** TODO *)
]

let level_up_tests = [
  level_up_test "leveling up starter pokemon"
    able_level_starter leveled_up_starter;
  level_up_test "leveling up grass pokemon"
    able_level_grass leveled_up_grass;
  level_up_test "leveling up water pokemon"
    able_level_water leveled_up_water;
]

let increase_exp_tests = [
  increase_exp_test "increase exp of starter bulbasaur with starter bulbasaur"
    test_starter_pkm test_starter_pkm increased_exp_starter;
  increase_exp_test "increase exp of grass charmander with grass charmander"
    test_grass_pkm test_grass_pkm increased_exp_grass;
  increase_exp_test "increase exp of water squirtle with water squirtle"
    test_water_pkm test_water_pkm increased_exp_water;
]
