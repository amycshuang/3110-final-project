(**
   Tests for all functions in Command.ml
*)
open OUnit2
open Command 
open Pokemon
open TestVariables

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let print_parse output = 
  match output with 
  | Quit -> "Quit"
  | Yes -> "Yes"
  | No -> "No"
  | Run -> "Run"
  | Catch -> "Catch"
  | Bag -> "Bag"
  | Map n -> "Map " ^ n
  | SPokemon p -> "Pokemon " ^ p
  | Attack lst -> "Attack " ^ pp_list pp_string lst
  | Battle lst -> "Battle " ^ pp_list pp_string lst 

let parse_test (name: string) (cmd: string) cmd_type
    expected_output : test = name >:: (fun _  -> 
    assert_equal expected_output (parse cmd cmd_type) ~printer: print_parse)

let parse_test_exn (name: string) (cmd: string) cmd_type
    (ex: exn) : test =  name >:: (fun _ -> 
    assert_raises ex (fun () -> parse cmd cmd_type))

let parse_region_tests = [
  (* testing non-exception *)
  parse_test "parsing valid region 'Kanto'" "Kanto" parse_region
    (Map "map1.json");
  parse_test "parsing valid region 'Johto'" "Johto" parse_region
    (Map "map1.json");
  parse_test "parsing valid region 'Hoenn'" "Hoenn" parse_region
    (Map "map1.json");
  parse_test "parsing valid region 'Sinnoh'" "Sinnoh" parse_region
    (Map "map1.json");
  parse_test "parsing valid region 'Unova'" "Unova" parse_region
    (Map "map1.json");
  parse_test "parsing valid region 'Johto'" "Kalos" parse_region
    (Map "map1.json");
  parse_test "parsing valid region 'Johto'" "Alola" parse_region
    (Map "map1.json");
  parse_test "parsing valid region 'Johto'" "Galar" parse_region
    (Map "map1.json");

  (* testing exception *)
  parse_test_exn "parsing invalid region 'Caml'" "Caml" 
    parse_region InvalidRegion;
  parse_test_exn "parsing invalid region 'kanto johto'" "Kanto Johto" 
    parse_region InvalidRegion;
  parse_test_exn "parsing invalid empty region ''" "" parse_region 
    InvalidRegion;
]

let parse_starter_tests = [
  (* testing non-exception *)
  parse_test "parsing valid starter 'charmander'" "charmander" parse_starter
    (SPokemon "charmander");
  parse_test "parsing valid starter 'bulbasaur'" "bulbasaur" parse_starter
    (SPokemon "bulbasaur");
  parse_test "parsing valid starter 'squirtle'" "squirtle" parse_starter
    (SPokemon "squirtle");
  parse_test "parsing valid starter 'pikachu'" "pikachu" parse_starter
    (SPokemon "pikachu");

  (* parsing exception *)
  parse_test_exn "parsing invalid starter 'pikachu pika'" "pikachu pika" 
    parse_starter (InvalidPokemon "Invalid Pokemon");
  parse_test_exn "parsing invalid starter 'giratina'" "giratina" parse_starter
    (InvalidPokemon "Invalid Pokemon");
  parse_test_exn "parsing invalid empty starter ''" "" parse_starter
    (InvalidCommand "Empty Command");
]

let parse_yn_tests = [
  (* testing non-exception *)
  parse_test "parsing valid 'yes'" "yes" parse_yn Yes;
  parse_test "parsing valid 'no'" "no" parse_yn No;

  (* testing exception *)
  parse_test_exn "parsing empty" "" parse_yn 
    (InvalidCommand "Empty Command");
  parse_test_exn "parsing invalid yes/no" "yes no" parse_yn 
    (InvalidCommand "Invalid Player Command");
]
