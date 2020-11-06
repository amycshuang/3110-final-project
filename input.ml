open Graphics
open Player

(** the type of user input data*)
type input = {
  pressed : bool;
  w : bool;
  a : bool;
  s : bool;
  d : bool;
}

(**[print_user_in u] prints the data of [u]*)
let print_user_in u = 
  "LMB: " ^ string_of_bool u.pressed |> print_endline;
  if u.w then print_string "w" else ();
  if u.a then print_string "a" else ();
  if u.s then print_string "s" else ();
  if u.d then print_string "d" else ();
  print_endline ""

(**[get_keys acc] returns a list of keys pressed*)
let rec get_keys acc =
  if key_pressed () 
  then get_keys (read_key () :: acc) 
  else acc 

let get_key () = (wait_next_event [Key_pressed]).Graphics.key

type move = Up | Left | Right | Down


(**[map_key ch] maps pressed character to an option of action*)
let map_key ch =
  match ch with
  | 'w' -> Some Up
  | 'a' -> Some Left
  | 's' -> Some Down
  | 'd' -> Some Right
  | _ -> None


