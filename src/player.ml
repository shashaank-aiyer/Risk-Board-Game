open Yojson.Basic.Util

type t = {
  new_troops : int;
  name : string;
  cards : Card.card list;
}

exception UnavailableTroops of int

let get_name player = player.name

let get_available_troops player = player.new_troops

let give_troops player troops =
  { player with new_troops = player.new_troops + troops }

let use_troops player troops =
  if troops > player.new_troops then raise (UnavailableTroops troops)
  else give_troops player (-troops)

let make_player name = { new_troops = 0; name; cards = [] }

let add_card card player = { player with cards = card :: player.cards }

let get_cards player = player.cards

let set_cards cards player = { player with cards }

let player_to_json player =
  `Assoc
    [
      ("new_troops", `Int player.new_troops);
      ("name", `String player.name);
      ("cards", `List (player.cards |> List.map Card.card_to_json));
    ]

let player_from_json json =
  {
    new_troops = json |> member "new_troops" |> to_int;
    name = json |> member "name" |> to_string;
    cards =
      json |> member "cards" |> to_list |> List.map to_string
      |> List.map Card.card_from_json;
  }
