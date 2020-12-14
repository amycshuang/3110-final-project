open OUnit2
open TestPokemon
open TestCommand


let pokemon_tests = List.flatten [
    TestPokemon.type_from_string_tests;
    TestPokemon.opponent_move_tests;
    TestPokemon.battle_damage_tests;
    TestPokemon.level_up_tests;
    TestPokemon.increase_exp_tests;
  ]

let command_tests = List.flatten [
    TestCommand.parse_region_tests;
    TestCommand.parse_starter_tests;
    TestCommand.parse_yn_tests;
  ]

let block_tests = List.flatten [
    TestBlock.block_tests;
  ]

let tests = List.flatten [
    pokemon_tests;
    command_tests;
    block_tests;
  ]

let suite = "test suite for Pokaml"  >::: tests

let _ = run_test_tt_main suite