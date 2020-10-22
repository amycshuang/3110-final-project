open OUnit2
open Pokemon

let pikachu = poke_from_json (Yojson.Basic.from_file "pikachu.json")

let getter_test (name: string) (f) (pokemon: Pokemon.t) (expected_output) : 
  test = name >:: (fun _ -> assert_equal expected_output (f pokemon))

let level_up_test (name: string) (pokemon: Pokemon.t)
    (expected_output: Pokemon.t) : 
  test = name >:: (fun _ -> assert_equal expected_output (level_up pokemon))

let pokemon_get_function_tests = 
  [
    getter_test "get_name is Pikachu" get_name pikachu "pikachu";
    getter_test "get_poke_type is Electric" get_poke_type pikachu 
      (type_from_string "Electric");
    getter_test "get_level is 5" get_level pikachu 5;
    getter_test "get_hp is 3" get_hp pikachu 3;
    getter_test "get_attack is 3" get_attack pikachu 3;
    getter_test "get_defense is 3" get_defense pikachu 3;
    getter_test "get_curr_exp is 0" get_curr_exp pikachu 0;
    getter_test "get_level_up_exp is 1" get_level_up_exp pikachu 1;
    getter_test "get_caught is false" get_caught pikachu false;
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    pokemon_get_function_tests;
  ]

let _ = run_test_tt_main suite