open OUnit2
open Pokemon


let pikachu = poke_from_json (Yojson.Basic.from_file "pikachu.json")

let getter_test (name: string) (f) (pokemon: Pokemon.t) (expected_output) : 
  test = name >:: (fun _ -> assert_equal expected_output (f pokemon))

let pokemon_get_function_tests = 
  [
    getter_test "get_name is Pikachu" get_name pikachu "pikachu";
    getter_test "get_poke_type is Electric" get_poke_type pikachu 
      (type_from_string "Electric");
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    pokemon_get_function_tests;

  ]

let _ = run_test_tt_main suite