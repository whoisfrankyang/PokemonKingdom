open Yojson.Basic.Util

type element =
  | Fire
  | Water
  | Grass
  | Poison
  | Electric
  | Ghost
  | Flying
  | Normal

module Skill = struct
  type skill = {
    name : string;
    damage : int;
    element : element;
  }

  let elem_of_string (elem : string) : element =
    match elem with
    | "Fire" -> Fire
    | "Water" -> Water
    | "Grass" -> Grass
    | "Poison" -> Poison
    | "Electric" -> Electric
    | "Ghost" -> Ghost
    | "Flying" -> Flying
    | "Normal" -> Normal
    | _ -> failwith "string doesn't match a element"

  let string_of_elem (elem : element) : string =
    match elem with
    | Fire -> "Fire"
    | Water -> "Water"
    | Grass -> "Grass"
    | Poison -> "Poison"
    | Electric -> "Electric"
    | Ghost -> "Ghost"
    | Flying -> "Flying"
    | Normal -> "Normal"

  (** Given [name], [damage],and [element], create a Pokemon skill Requires:
      [damage > 0]*)
  let create_skill name damage element =
    { name : string; damage : int; element : element }

  let skills_master_lst =
    let load_jsons =
      Yojson.Basic.from_file "data/skill.json" |> member "skills" |> to_list
    in
    (*convert Yojson.Basic.t to skill*)
    let decode_json json =
      let name = json |> member "name" |> to_string in
      let damage = json |> member "damage" |> to_int in
      let element = json |> member "element" |> to_string |> elem_of_string in
      create_skill name damage element
    in
    List.map decode_json load_jsons

  (** Use when decoding pokemon.json, for string list in skill field, convert to
      Skill list*)
  let construct_skills (lst : string list) : skill list =
    let rec build_lst lst acc =
      match lst with
      | [] -> acc (*h is string, x is skill*)
      | h :: t ->
          List.find (fun x -> x.name = h) skills_master_lst :: build_lst t acc
    in
    build_lst lst []

  let get_skill skill = List.find (fun p -> p.name = skill) skills_master_lst
end

module Pokemon = struct
  type pokemon = {
    name : string;
    mutable hp_curr : int;
    hp_max : int;
    mutable level : int;
    mutable xp : int;
    elem_type : element;
    mutable skills : Skill.skill list;
  }

  let type_advantage (elem : element) : element list =
    match elem with
    | Fire -> [ Grass ]
    | Water -> [ Fire ]
    | Grass -> [ Water ]
    | Poison -> [ Grass ]
    | Electric -> [ Water; Flying ]
    | Flying -> [ Poison ]
    | Normal -> []
    | Ghost -> [ Ghost ]

  let type_disadvantage (elem : element) : element list =
    match elem with
    | Fire -> [ Water ]
    | Water -> [ Grass ]
    | Grass -> [ Fire; Poison ]
    | Poison -> [ Flying ]
    | Electric -> []
    | Flying -> [ Electric ]
    | Normal -> []
    | Ghost -> [ Ghost ]

  (** Return true if [skill] has type advantage over [other_pokemon], else
      return false*)
  let is_advantage (skill : Skill.skill) (other_pokemon : pokemon) =
    List.exists
      (fun x -> x = other_pokemon.elem_type)
      (type_advantage skill.element)

  (** Return true if [skill] has type disadvantage to [other_pokemon], else
      return false*)
  let is_disadvantage (skill : Skill.skill) (other_pokemon : pokemon) =
    List.exists
      (fun x -> x = other_pokemon.elem_type)
      (type_disadvantage skill.element)

  let print_hp pokemon =
    print_endline
      (pokemon.name ^ " HP: "
      ^ string_of_int pokemon.hp_curr
      ^ "/"
      ^ string_of_int pokemon.hp_max
      ^ "\n")

  let attack own_pokemon other_pokemon skill =
    let base_damage = skill.Skill.damage in
    let random_range = Random.int 4 in
    let multiplier =
      if is_advantage skill other_pokemon then 1.5
      else if is_disadvantage skill other_pokemon then 0.5
      else 1.0
    in
    let calculated_damage =
      int_of_float (multiplier *. float_of_int base_damage) + random_range
    in
    if calculated_damage <= 0 then (
      print_endline
        (other_pokemon.name ^ " dodged "
        ^ String.lowercase_ascii skill.name
        ^ ". No damage caused.");
      print_hp other_pokemon)
    else
      let new_hp_curr = max 0 (other_pokemon.hp_curr - calculated_damage) in
      other_pokemon.hp_curr <- new_hp_curr;
      print_endline
        (own_pokemon.name ^ "'s " ^ skill.name ^ " caused "
        ^ string_of_int calculated_damage
        ^ " damage to " ^ other_pokemon.name);
      print_hp other_pokemon

  let defend own_pokemon other_pokemon skill =
    let base_damage = skill.Skill.damage in
    let defense_value = 0 in
    let reduced_damage = max 0 (base_damage - defense_value) in
    let new_hp_curr = max 0 (own_pokemon.hp_curr - reduced_damage) in
    own_pokemon.hp_curr <- new_hp_curr

  let level_up pokemon =
    let gained_xp = 1 + Random.int 10 in
    let prompt = pokemon.name ^ " gained " ^ string_of_int gained_xp ^ " XP." in
    let new_xp = pokemon.xp + gained_xp in
    if new_xp >= 20 then (
      print_endline prompt;
      print_endline ("[ XP: " ^ string_of_int (new_xp - 20) ^ "/20 ]");
      print_endline
        (pokemon.name ^ " reached Level " ^ string_of_int (pokemon.level + 1));
      let leveled_skills =
        List.map
          (fun skill -> { skill with Skill.damage = skill.Skill.damage + 5 })
          pokemon.skills
      in
      let updated_hp_max = pokemon.hp_max + 5 in
      {
        pokemon with
        level = pokemon.level + 1;
        xp = new_xp - 20;
        skills = leveled_skills;
        hp_max = updated_hp_max;
      })
    else (
      print_endline prompt;
      print_endline ("[ XP: " ^ string_of_int new_xp ^ "/20 ]");
      { pokemon with xp = new_xp })

  let construct_pokemon (name : string) (hp_curr : int) (hp_max : int)
      (level : int) (xp : int) (elem_type : element) (skills : Skill.skill list)
      =
    if hp_curr < 0 || hp_max < 0 then raise (Failure "HP cannot be negative")
    else if hp_curr > hp_max then
      raise (Failure "Current HP cannot be greater than max HP")
    else if level < 1 then raise (Failure "Level must be at least 1")
    else if xp < 0 then raise (Failure "XP must be at least 0")
    else if skills = [] then
      raise (Failure "Pokemon must have at lease one skill")
    else { name; hp_curr; hp_max; level; xp; elem_type; skills }

  (** decode a json instance by its identifiers*)
  let decode_json json =
    let name = json |> member "name" |> to_string in
    let hp = json |> member "hp" |> to_int in
    let level = 1 in
    let xp = 0 in
    let elem = json |> member "elem" |> to_string |> Skill.elem_of_string in
    let skills =
      json |> member "skills" |> to_list |> List.map to_string
      |> Skill.construct_skills
    in
    construct_pokemon name hp hp level xp elem skills

  let masterlist =
    let poke_json_lst =
      let json = Yojson.Basic.from_file "data/pokemon.json" in
      json |> member "pokemons" |> to_list
    in
    List.map decode_json poke_json_lst
end
