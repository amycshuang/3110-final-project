(** 
   Tests for functions in Trainer.ml
*)
open OUnit2
open Trainer
open Pokemon

let trainer_list = 
  trainer_list_from_json (Yojson.Basic.from_file "trainers.json")

let trainer_tests = [
  "name of first trainer is Clarkson"  >:: (fun _ -> 
      assert_equal "Clarkson" (List.hd trainer_list).name);

  "catchphrase of second trainer"  >:: (fun _ -> 
      assert_equal "Brace yourself, it's time to see if you're a real CS nerd." 
        (List.hd (List.nth trainer_list 1).catchphrase))
]