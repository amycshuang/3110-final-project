open Pokemon

type phrase = string list

type command = 
  | Quit
  | Yes
  | No
  | Run 
  | Catch 
  | Bag
  | Map of string
  | SPokemon of string
  | Attack of phrase
  | Battle of phrase

exception InvalidRegion

exception InvalidCommand of string

let parse_region region = 
  match region with 
  | [] -> raise InvalidRegion
  | h :: t -> begin 
      match h with 
      | "kanto" -> Map "map1.json"
      | "johto" -> Map "map1.json"
      | "hoenn" -> Map "map1.json"
      | "sinnoh" -> Map "map1.json"
      | "unova" -> Map "map1.json"
      | "kalos" -> Map "map1.json"
      | "alola" -> Map "map1.json"
      | "galar" -> Map "map1.json"
      | _ -> raise InvalidRegion
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