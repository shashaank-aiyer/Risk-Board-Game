(** - list of all cards and functions*)

type card =
  | Soldier
  | Horse
  | Tank

val give_card : unit -> card
(** [give card] returns a random card to give to a player upon winning
    their first attack of a turn *)

val trade : card list -> card list
(* [trade] takes in a card list and returns a new card list with the
   cards that have been traded in removed *)

val card_to_json : card -> Yojson.Basic.t

val card_from_json : string -> card