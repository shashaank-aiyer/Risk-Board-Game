type t

exception IllegalAttack


type dice_roll = {
  attack : int list;
  defend : int list;
  attack_num_lost : int;
  defend_num_lost : int;
}

(* [create_attack] takes in a state, attacking country, and defending
   country and creates an attack of type t containing the given
   information*)
val create_attack :
  Board.board -> string -> Board.country_id -> Board.country_id -> t

(* [roll_dice takes in an attack and returns a dice_roll]*)
val roll_dice : t -> dice_roll
