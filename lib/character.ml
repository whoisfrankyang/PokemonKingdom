open Pokemon

module BackPack = struct
  type backpack = {
    current_capacity : int;
    max_capacity : int;
    potions : int;
    revives : int;
  }
end

module Character = struct
  type character = {
    character_name : string;
    pokemon_list : Pokemon.pokemon list;
    backpack : BackPack.backpack;
  }

  let init name =
    {
      character_name = name;
      pokemon_list = [];
      backpack =
        { current_capacity = 0; max_capacity = 10; potions = 0; revives = 0 };
    }

  let add_pokemon character pokemon =
    if
      List.exists
        (fun p -> p.Pokemon.name = pokemon.Pokemon.name)
        character.pokemon_list
    then character
    else { character with pokemon_list = pokemon :: character.pokemon_list }

  let remove_pokemon character pokemon_name =
    {
      character with
      pokemon_list =
        List.filter
          (fun p -> p.Pokemon.name <> pokemon_name)
          character.pokemon_list;
    }

  let get_pokemon character pokemon_name =
    List.find (fun p -> p.Pokemon.name = pokemon_name) character.pokemon_list

  let add_item character item (amount : int) =
    let new_capacity = character.backpack.current_capacity + amount in
    let add_amount =
      if amount < 0 then raise (Failure "Error: Amount cannot be negative")
      else if new_capacity > character.backpack.max_capacity then
        character.backpack.max_capacity - character.backpack.current_capacity
      else amount
    in
    let overflow_msg name =
      let add_msg =
        if add_amount = 0 then "No item added."
        else "Only " ^ string_of_int add_amount ^ " " ^ name ^ " added."
      in
      if amount <> add_amount then
        print_endline ("Your backpack doesn't have enough capacity! " ^ add_msg)
      else
        print_endline
          ("****Added [ " ^ string_of_int add_amount ^ " "
          ^ String.capitalize_ascii name
          ^ " ] to your backpack****")
    in
    match item with
    | "potion" ->
        overflow_msg "potion";
        {
          character with
          backpack =
            {
              character.backpack with
              potions = character.backpack.potions + add_amount;
              current_capacity = new_capacity;
            };
        }
    | "revive" ->
        overflow_msg "revive";
        {
          character with
          backpack =
            {
              character.backpack with
              revives = character.backpack.revives + add_amount;
              current_capacity = new_capacity;
            };
        }
    | _ -> failwith "Invalid item"

  let remove_item character item =
    let new_capacity = character.backpack.current_capacity - 1 in
    if new_capacity < 0 then
      raise (Failure "Error: Current capacity can't be negative!")
    else
      match item with
      | "potion" when character.backpack.potions > 0 ->
          {
            character with
            backpack =
              {
                character.backpack with
                potions = character.backpack.potions - 1;
                current_capacity = new_capacity;
              };
          }
      | "revive" when character.backpack.revives > 0 ->
          {
            character with
            backpack =
              {
                character.backpack with
                revives = character.backpack.revives - 1;
                current_capacity = new_capacity;
              };
          }
      | "potion" | "revive" -> raise (Failure "Not enough items to use.")
      | _ -> failwith "Invalid item"

  (** [revert_hp character] revert the HP of all the pokemons in the pokemon
      list of [player] to its max HP*)
  let revert_hp character =
    let healed_pokemons =
      List.map
        (fun pok -> { pok with Pokemon.hp_curr = pok.Pokemon.hp_max })
        character.pokemon_list
    in
    { character with pokemon_list = healed_pokemons }

  let heal_all character =
    print_endline "Let's inspect all your pokemons: ";
    List.iter
      (fun pok ->
        if pok.Pokemon.hp_curr != pok.Pokemon.hp_max then
          print_endline
            ("   Healing " ^ pok.name ^ " ... Done! HP healed from "
           ^ string_of_int pok.hp_curr ^ " to " ^ string_of_int pok.hp_max)
        else print_endline ("   " ^ pok.name ^ " has full HP..."))
      character.pokemon_list;
    print_endline "All pokemons have been inspected and healed!";
    revert_hp character
end
