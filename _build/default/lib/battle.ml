open Pokemon
open Character
open Skill

let posHP_pokemon (pList : Pokemon.pokemon list) =
  List.filter (fun p -> p.Pokemon.hp_curr > 0) pList

(** Allow user to quit if there exist no pokemon in the list with hp_curr>0*)
let rec choose_pokemon (pList : Pokemon.pokemon list) =
  try
    let pList = posHP_pokemon pList in
    match pList with
    | [] -> raise Not_found (*not possible*)
    | h :: t ->
        Story.StoryPlot.pp_lines
          "Here are the available Pokemons in your backpack (choose the \
           pokemon by typing their corresponding number):  ";
        for i = 0 to List.length pList - 1 do
          let pok = List.nth pList i in
          print_endline
            (string_of_int (i + 1)
            ^ ". " ^ pok.name ^ "  Element: "
            ^ string_of_elem pok.elem_type
            ^ "  HP: " ^ string_of_int pok.hp_curr ^ "/"
            ^ string_of_int pok.hp_max)
        done;
        let decision = read_line () in
        List.nth pList (int_of_string decision - 1)
  with
  | Not_found -> raise Not_found
  | Failure _ ->
      print_endline
        "That's not a valid number. Please enter a valid Pokemon number.";
      choose_pokemon pList

let generate_pokemon (masterlist : Pokemon.pokemon list) =
  let index = Random.int (List.length masterlist) in
  let my_pokemon = List.nth masterlist index in
  let new_pokemon =
    {
      Pokemon.name = my_pokemon.name;
      Pokemon.hp_curr = my_pokemon.hp_curr;
      Pokemon.hp_max = my_pokemon.hp_max;
      Pokemon.level = my_pokemon.level;
      Pokemon.xp = my_pokemon.xp;
      Pokemon.elem_type = my_pokemon.elem_type;
      Pokemon.skills = my_pokemon.skills;
    }
  in
  new_pokemon

(** TODO: allow user to quit *)
let take_turn (character : Character.character) (pokemon1 : Pokemon.pokemon)
    (pokemon2 : Pokemon.pokemon) =
  while pokemon1.hp_curr > 0 && pokemon2.hp_curr > 0 do
    print_endline "Choose a skill by entering the corresponding skill number: ";
    let skill_name1 =
      List.map (fun s -> s.Skill.name) pokemon1.Pokemon.skills
    in
    let skill_damage1 =
      List.map (fun s -> s.Skill.damage) pokemon1.Pokemon.skills
    in
    let skill_element1 =
      List.map (fun s -> s.Skill.element) pokemon1.Pokemon.skills
    in
    let skill_name2 =
      List.map (fun s -> s.Skill.name) pokemon2.Pokemon.skills
    in
    let combined_skills1 =
      List.map2 (fun name damage -> (name, damage)) skill_name1 skill_damage1
    in
    let combined_skills =
      List.map2
        (fun (name, damage) element -> (name, damage, element))
        combined_skills1 skill_element1
    in
    List.iteri
      (fun i (name, damage, element) ->
        print_endline
          (string_of_int (i + 1)
          ^ ". " ^ name ^ " [" ^ string_of_int damage ^ "] ["
          ^ string_of_elem element ^ "]"))
      combined_skills;
    try
      let decision = read_line () in
      let index = int_of_string decision - 1 in
      if index >= 0 && index < List.length pokemon1.Pokemon.skills then (
        let skill1 = List.nth pokemon1.Pokemon.skills index in
        let skill2 =
          List.nth pokemon2.Pokemon.skills
            (Random.int (List.length skill_name2))
        in
        Pokemon.attack pokemon1 pokemon2 skill1;
        Pokemon.attack pokemon2 pokemon1 skill2)
      else print_endline "Invalid skill number."
    with
    | Failure _ ->
        print_endline
          "That's not a valid number. Please enter a valid skill number."
    | _ -> print_endline "An unexpected error occurred."
  done

type battle_result =
  | Win of Character.character
  | Lose of unit

let battle (character : Character.character ref) : battle_result =
  let opponent = generate_pokemon Pokemon.masterlist in
  let line =
    "\n *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* \n"
  in
  let prompt =
    line ^ "You initiate a battle with [" ^ opponent.name ^ "| Element: "
    ^ string_of_elem opponent.elem_type
    ^ " | Max HP: "
    ^ string_of_int opponent.hp_max
    ^ "]"
  in
  let pok =
    ref
      (Pokemon.construct_pokemon "" 0 0 1 0 Normal
         [ Skill.create_skill "name" 0 Normal ])
  in
  try
    print_endline prompt;
    pok := choose_pokemon !character.pokemon_list;
    take_turn !character !pok opponent;
    if opponent.hp_curr = 0 then (
      print_endline ("[You won]" ^ line);
      pok := Pokemon.level_up !pok;
      let same_pokemon_exists =
        List.exists
          (fun (x : Pokemon.pokemon) -> x.name = opponent.name)
          !character.pokemon_list
      in
      let same_pokemon_exists2 =
        List.exists
          (fun (x : Pokemon.pokemon) -> x.name = !pok.name)
          !character.pokemon_list
      in
      let character_pok_list =
        if same_pokemon_exists then !character.pokemon_list else
          (* List.filter
            (fun (x : Pokemon.pokemon) ->
              if x.name <> !pok.name then true else false) *)
            !character.pokemon_list @ [ opponent ]
      in
      (* let new_pok_list = character_pok_list @ [ opponent ] in  *)
      let new_chara : Character.character =
        if same_pokemon_exists2 then 
        {
          character_name = !character.character_name;
          pokemon_list = (*!pok ::*) character_pok_list;
          (* pokemon_list = character_pok_list; *)
          backpack = !character.backpack;
        }
        else 
          {
          character_name = !character.character_name;
          pokemon_list = !pok :: character_pok_list;
          (* pokemon_list = character_pok_list; *)
          backpack = !character.backpack;
        }
      in
      Win new_chara)
    else Lose (print_endline ("[You lost]" ^ line))
  with Not_found ->
    Lose
      (print_endline
         "[Failure] No pokemon with positive hp, please visit hospital")
