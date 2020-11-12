open OUnit2
open Command
open Pokemon

let pikachu = poke_from_json (Yojson.Basic.from_file "pikachu.json")

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
  | Attack lst -> "Attack" ^ pp_list pp_string lst
  | Battle lst -> "Battle" ^ pp_list pp_string lst 

let parse_test (name: string) (pkm: Pokemon.t option) (cmd: string) 
    (expected_output: command) : test = name >:: (fun _  -> 
    assert_equal expected_output (parse pkm cmd) ~printer: print_parse)

let parse_test_exn (name: string) (pkm: Pokemon.t option) (cmd: string) 
    (ex: exn) : test =  name >:: (fun _ -> 
    assert_raises ex (fun () -> parse pkm cmd))

let command_tests =
  [
    parse_test "parse 'quit'" None "quit" Quit;
    parse_test "parse 'attack with thunderbolt'" (Some pikachu)
      "attack with thunderbolt" (Attack["thunderbolt"]);

    parse_test_exn "invalid attack parse" 
      (Some pikachu) "attack with flamethrower" 
      (InvalidAttack "Invalid Pokemon Attack flamethrower");
  ]