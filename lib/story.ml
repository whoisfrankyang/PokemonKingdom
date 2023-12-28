open Pokemon
open Yojson.Basic.Util

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

module StoryPlot = struct
  type plot = string tree

  (** [get_bag_detail bag] print details each field in [bag]*)
  let get_bag_detail (bag : Character.BackPack.backpack) : unit list =
    [
      print_endline
        ("Capacity: "
        ^ string_of_int bag.current_capacity
        ^ " / "
        ^ string_of_int bag.max_capacity);
      print_endline ("Number of Potions: " ^ string_of_int bag.potions);
      print_endline ("Number of Revives: " ^ string_of_int bag.revives);
    ]

  (** [view_bag character] Print details of [character]'s backpack *)
  let view_bag (char : Character.Character.character) : unit =
    print_endline "\n***********Viewing Backpack***********";
    List.iter (fun p -> p) (get_bag_detail char.backpack);
    print_endline "*************************************\n "

  (** [string_of_elem elem] takes in a pokemon.elem and returns the string
      representation of that element *)
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

  let split_words s = Str.split (Str.regexp "[ \t\n\r]+") s

  (** [wrap_text text width] creates a string list, where each string represent
      a line. Wrap [text] according to [width] so that no words are cut off.
      Example: [wrap_text "Time stands still in sunsets" 6] =
      ["City lights";"dance after"; "twilight"] *)
  let wrap_text text width =
    let words_lst = split_words text in
    let rec wrap curr_line curr_length = function
      | [] -> [ curr_line ]
      | h :: t ->
          let word_length = String.length h in
          if curr_length + word_length <= width then
            wrap (curr_line ^ " " ^ h) (curr_length + word_length + 1) t
          else curr_line :: wrap h word_length t
    in
    wrap "" 0 words_lst

  let pp_lines text =
    let width = 70 in
    List.iter print_endline (wrap_text text width)

  (** [get_layer lst] calculate the number of layer of tree from given stirng
      lst Requires: [lst_len] = (2^n) - 1 *)
  let get_layer lst_len =
    int_of_float (log (float_of_int (lst_len + 1)) /. log 2.0)

  let construct_plot (list : string list) : 'a tree =
    let rec insert_tree (lst : 'a list) (acc : int) : 'a tree * 'a list =
      match (lst, acc) with
      | [], _ -> (Leaf "End", [])
      | h :: t, 0 -> (Leaf "End", h :: t)
      | h :: t, a ->
          let left, rem_after_left = insert_tree t (acc - 1) in
          let right, rem_after_right = insert_tree rem_after_left (acc - 1) in
          (Node (h, left, right), rem_after_right)
    in
    match list with
    | [] -> Leaf "End"
    | h :: t ->
        let tree, _ = insert_tree (h :: t) (get_layer (List.length list)) in
        tree

  (** Process line for game, if string contains [#], extract string after [#],
      else return string. Example: [process_head "head"] = "head",
      [process_head "I walked to | town"] = "town" *)
  let process_head line =
    try
      let key_index = String.index line '|' in
      String.sub line (key_index + 1) (String.length line - key_index - 1)
    with Not_found -> line

  (** Process line for game, if string contains [#], extract string before [#],
      else return string. Example: [process_tail "head"] = "head",
      [process_tail "I walked to | town"] = "I walked to" *)
  let process_tail line =
    try
      let key_index = String.index line '|' in
      String.sub line 0 key_index
    with Not_found -> line

  (** [rand_lst plot] gives the string list extracted from json for [plot]*)
  let rand_lst plot =
    let load_lst plot =
      let json = Yojson.Basic.from_file "data/random.json" in
      json |> member plot |> to_list |> List.map to_string
    in
    load_lst plot

  (** [detect_random line] extract string from [line] after '@, the string
      represents an identifier for desired information. Corresponding data
      extracted from json file, and one string is selected at random from list.*)
  let detect_random line =
    try
      let key_index = String.index line '@' in
      let lst =
        rand_lst
          (String.sub line (key_index + 1) (String.length line - key_index - 1))
      in
      List.nth lst (Random.int (List.length lst))
    with Not_found -> line

  let out_put (pl : plot) : unit =
    match pl with
    | Leaf l -> print_endline l
    | Node (h, l, r) -> (
        print_endline "";
        (*if h index_opt has some *)
        pp_lines (detect_random h |> process_head);
        (match l with
        | Leaf l -> print_endline l
        | Node (hl, ll, rl) -> print_endline ("A: " ^ process_tail hl));
        match r with
        | Leaf r -> ()
        | Node (hr, lr, rr) -> print_endline ("B: " ^ process_tail hr))

  let get_string (pl : plot) : string =
    match pl with
    | Leaf l -> l
    | Node (h, l, r) -> h

  (** [get_detail pok] print details such as name, level, element, etc for [pok]*)
  let get_detail (pok : Pokemon.pokemon) : unit list =
    [
      List.iter print_endline
        (List.map
           (fun skill -> "  - " ^ skill)
           (let rec skill_list (lst : Skill.skill list) : string list =
              match lst with
              | [] -> []
              | h :: t ->
                  (h.name ^ " | " ^ string_of_elem h.element ^ " | Damage: "
                 ^ string_of_int h.damage)
                  :: skill_list t
            in
            skill_list pok.skills));
      print_endline "Skills: ";
      print_endline
        ("HP: " ^ string_of_int pok.hp_curr ^ "/" ^ string_of_int pok.hp_max);
      print_endline ("Element: " ^ string_of_elem pok.elem_type);
      print_endline ("XP: " ^ string_of_int pok.xp ^ "/20");
      print_endline ("Level: " ^ string_of_int pok.level);
      print_endline ("Name: " ^ pok.name);
    ]

  let view_pokemon (pok : Pokemon.pokemon) : unit =
    print_endline "\n***********Viewing Pokemon************";
    List.iter (fun p -> p) (get_detail pok);
    print_endline "*************************************\n "

  let pp_pokemon_lst (lst : Pokemon.pokemon list) : unit =
    let pp_pokemon =
      List.map
        (fun pok ->
          let split = print_endline "*************************************" in
          let detail = get_detail pok @ [ split ] in
          List.iter (fun p -> p) detail)
        lst
    in
    List.iter (fun p -> p) pp_pokemon

  let search_pokemon (name : string) (lst : Pokemon.pokemon list) :
      Pokemon.pokemon =
    let pok = List.find (fun p -> p.Pokemon.name = name) lst in
    Pokemon.construct_pokemon pok.name pok.hp_max pok.hp_max 1 0 pok.elem_type
      pok.skills

  let starter_poke : Pokemon.pokemon list =
    let rec create poke_lst acc =
      match poke_lst with
      | [] -> acc
      | h :: t -> search_pokemon h Pokemon.masterlist :: create t acc
    in
    create [ "Pikachu"; "Squirtle"; "Charmander"; "Bulbasaur" ] []

  let rec make_decision (pl : plot) (dec : string)
      (player : Character.Character.character) : plot =
    match pl with
    | Leaf l -> Leaf l
    | Node (h, l, r) ->
        if dec = "A" then l
        else if dec = "B" then r
        else if dec = "O" then begin
          ignore (view_bag player);
          out_put pl;
          make_decision pl (read_line ()) player
        end
        else if dec = "L" then begin
          if List.length player.pokemon_list != 0 then (
            print_endline "\n Here is a list of your pokemons:";
            pp_pokemon_lst player.pokemon_list;
            print_endline "*************************************")
          else print_endline "\n   [You currently have no pokemon]";
          out_put pl;
          make_decision pl (read_line ()) player
        end
        else if dec = "P" then begin
          print_endline "\n Here is all the pokemon in this game: ";
          pp_pokemon_lst Pokemon.masterlist;
          print_endline "*************************************";
          out_put pl;
          make_decision pl (read_line ()) player
        end
        else if dec = "S" then begin
          print_endline "*************************************";
          print_endline "Enter Pokemon Name: ";
          let rec view =
            try view_pokemon (search_pokemon (read_line ()) Pokemon.masterlist)
            with Not_found ->
              print_endline "\n   [Pokemon name you enter is not found!]"
          in
          view;
          out_put pl;
          make_decision pl (read_line ()) player
        end
        else if dec = "E" then Leaf "End"
        else
          (print_endline
             "\n\
             \ [It seems like you have accidentally press a different key, try \
              again!]";
           make_decision pl (read_line ()))
            player
end
