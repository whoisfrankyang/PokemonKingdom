(** Element determines Pokemon's strength and weakness in attack and defense *)
type element =
  | Fire
  | Water
  | Grass
  | Poison
  | Electric
  | Ghost
  | Flying
  | Normal

(** [Skill] outlines the power/ability a pokemon possesses in a battle *)
module Skill : sig
  type skill = {
    name : string;
    damage : int;
    element : element;
  }

  val skills_master_lst : skill list
  (** Master list of all skills in the game*)

  val elem_of_string : string -> element
  (** [elem_of_string str] converts [str] to its element representation. If
      [str] is an invalid representation, then raise [Failure]*)

  val string_of_elem : element -> string
  (** [string_of_elem elm] converts [elm] to its string representation *)

  val create_skill : string -> int -> element -> skill
  (** [create_skill name damage elm] create a Pokemon skill with name [name],
      damage [damage], and element [elm]. Requires: [damage > 0] and [elm] be a
      valid element*)

  val get_skill : string -> skill
  (** [get_skill str] return skill with name [str]. Raise [Not Found] if no
      skill exist with that name*)
end

(** A model for [Pokemon] creature which engage in battles *)
module Pokemon : sig
  (*[pokemon] defines what characteristics a Pokemon possesses*)
  type pokemon = {
    name : string;
    mutable hp_curr : int;
    hp_max : int;
    mutable level : int;
    mutable xp : int;
    elem_type : element;
    mutable skills : Skill.skill list;
  }

  val masterlist : pokemon list
  (** Master list of all pokemon in the game*)

  val construct_pokemon :
    string -> int -> int -> int -> int -> element -> Skill.skill list -> pokemon
  (** [construct_pokemon name hp_curr hp_max level xp elm_type skills] construct
      a pokemon given its relevant info Raises [Failure] if hp_curr or hp_max
      negative, hp_curr greater than hp_max, level < 1, xp < 0, or skills is
      empty list.*)

  val attack : pokemon -> pokemon -> Skill.skill -> unit
  (** [attack own other skill] calculate damage to [other] caused by [own]
      attacking using [skill]. Requires: [skill] must be in Skill list of [own] *)

  val defend : pokemon -> pokemon -> Skill.skill -> unit
  (** [defend own other skill] calculate damage reduction of [own] when
      defending against [other] using [skill]. Requires: [skill] must be in
      Skill list of [other]*)

  val level_up : pokemon -> pokemon
  (** [level_up pokemon] add ramdom amount XP to [pokemon] and increment level
      of [pokemon] by 1 if XP exceed 20*)

  val type_advantage : element -> element list
  (** [type_advantage elm] give the list of element that [elm] has advantage
      over. If returns [[]] then [elm] has no type advantage over any element.
      Example: [type_advantage Fire] gives [[Grass]]. [type_advantage Normal]
      gives [[]]*)

  val type_disadvantage : element -> element list
  (** [type_disadvantage elm] give the list of element that [elm] has
      disadvantage to. If returns [[]] then [elm] has no type disadvantage to
      any element. Example: [type_disadvantage Fire] gives [[Water]].
      [type_advantage Normal] gives [[]]*)
end
