open Pokemon

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

(** [StoryPlot] has the type of a string binary tree that holds the main plot.*)
module StoryPlot : sig
  type plot = string tree

  val construct_plot : string list -> plot
  (** [construct_plot lst] is a plot tree from a given list of strings.
      Requires: [list] must be in preorder. *)

  val make_decision : plot -> string -> Character.Character.character -> plot
  (** [make_decision pl dec player] user will make a decision between two
      choices, the function will returns the corresponding subtree for the
      duration of the rest of the game. Requires: dec must be either "A" or "B" *)

  val out_put : plot -> unit
  (** [out_put pl] prints out the string of the current plot along with the two
      possible choices offer to the user and/or the notion that the game is now
      ended with "End" *)

  val get_string : plot -> string
  (** [get string pl] is the string of the plot [pl]*)

  val pp_lines : string -> unit
  (** [pp_lines text] print wrapped [text] for terminal*)

  val starter_poke : Pokemon.pokemon list
  (** Returns list of starter pokemons in game*)

  val view_pokemon : Pokemon.pokemon -> unit
  (** [view_pokemon pok] Print details about a given [Pokemon.pokemon],
      including its skills, HP, element type, level, and name. *)

  val search_pokemon : string -> Pokemon.pokemon list -> Pokemon.pokemon
  (** [search_pokemon name lst] Get a Pokemon from the pokemon list of [lst] by
      name of [name] Raise: [Not_Found] if given pokemon name does not exist in
      the provided list *)

  val view_bag : Character.Character.character -> unit
  (** [view_bag character] Print details of [character]'s backpack*)

  val pp_pokemon_lst : Pokemon.pokemon list -> unit
  (** [pp_pokemon_lst lst] Print details about each [Pokemon.pokemon] in [lst] *)
end
