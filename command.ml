open Pokemon

type phrase = string list

type command = 
  | Quit
  | Yes
  | No
  | Run 
  | Catch 
  | Bag
  | SPokemon of string
  | Attack of phrase
  | Battle of phrase

exception Impossible 

exception InvalidAttack of string

exception InvalidCatch of string

exception InvalidCommand of string

(** [check_valid_catch pkm] is true if it is valid to catch pokemon 
    [pkm], false otherwise. *)
let check_valid_catch = function
  | None -> raise Impossible
  | Some p -> get_caught p

(** [get_pkm_move mv_lst] is the string representation of a pokemon move
    formed by catenating each element of the string list [mv_lst] together *)
let rec get_pkm_move = function 
  | [] -> ""
  | h :: t -> h ^ get_pkm_move t

(** [check_attack_move pkm atk] is true if [atk] is a valid attack for the 
    pokemon [pkm]. *)
let check_attack_move  move = function 
  | None -> raise Impossible
  | Some p -> valid_move_name p move

(** TODO: implement check to determine if valid battle 
    -idea: maybe check that the player is at most 1 square away from a trainer
    or gym leader to battle? *)

(** [check_valid_battle valid_battle] is true if the player is able to battle, 
    false otherwise. *)
let check_valid_battle valid_battle = failwith "Unimplemented"

let parse_cmd pkm cmd = 
  match cmd with 
  | [] -> raise (InvalidCommand "Empty Command")
  | h :: t -> begin 
      match h, t with 
      | "quit", [] -> Quit
      | "yes", [] -> Yes
      | "no", [] -> No 
      | "run", [] -> Run
      | "catch", [] -> 
        if check_valid_catch pkm 
        then Catch
        else raise (InvalidCatch "Cannot Catch This Pokemon")
      | "bag", [] -> Bag
      | "attack", [] -> raise (InvalidAttack "Invalid Attack Command")
      | "attack", t -> 
        if check_attack_move (get_pkm_move t) pkm
        then Attack t 
        else raise (InvalidAttack ("Invalid Pokemon Attack " ^ get_pkm_move t))
      | "battle", [] -> raise (InvalidCommand "Invalid Battle Command")
      | "battle", t -> 
        if check_valid_battle true 
        then Battle t
        else raise (InvalidCommand "Invalid Battle Command")
      | _ -> raise (InvalidCommand "Invalid Player Command")
    end

let parse_starter pkm =
  match pkm with 
  | [] -> raise (InvalidCommand "Empty Command")
  | h :: t -> begin 
      match h with 
      | "charmander" as c -> SPokemon c
      | "bulbasaur" as b -> SPokemon b
      | "squirtle" as s -> SPokemon s
      | "pikachu" as p -> SPokemon p
      | _ -> raise (InvalidPokemon "Invalid Pokemon")
    end 

let parse_yn cmd = 
  match cmd with 
  | [] -> raise (InvalidCommand "Empty Command")
  | h :: t -> begin 
      match h, t with 
      | "yes", [] -> Yes
      | "no", [] -> No 
      | _ -> raise (InvalidCommand "Invalid Player Command")
    end

let parse cmd cmd_type =
  cmd 
  |> String.lowercase_ascii
  |> String.split_on_char ' ' 
  |> List.filter (fun x -> x <> "" && x <> "with")
  |> cmd_type