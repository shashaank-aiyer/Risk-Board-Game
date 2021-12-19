type card =
  | Soldier
  | Horse
  | Tank

type card_record = {
  soldier : int;
  horse : int;
  tank : int;
}

exception InvalidCard

exception InvalidTrade

let rec add_n_cards card n lst =
  match n with
  | 0 -> lst
  | _ -> add_n_cards card (n - 1) (card :: lst)

let give_card () =
  match Random.int 3 with
  | 0 -> Soldier
  | 1 -> Horse
  | 2 -> Tank
  | _ -> raise InvalidCard

let new_cards cardrec =
  add_n_cards Soldier cardrec.soldier []
  |> add_n_cards Horse cardrec.horse
  |> add_n_cards Tank cardrec.tank

let rec trade_helper cardlst acc =
  match cardlst with
  | [] -> acc
  | h :: t -> (
      match h with
      | Soldier -> trade_helper t { acc with soldier = acc.soldier + 1 }
      | Horse -> trade_helper t { acc with horse = acc.horse + 1 }
      | Tank -> trade_helper t { acc with tank = acc.tank + 1 })

let trade cardlst =
  match trade_helper cardlst { soldier = 0; horse = 0; tank = 0 } with
  | cardrec ->
      if cardrec.soldier >= 3 then
        new_cards { cardrec with soldier = cardrec.soldier - 3 }
      else if cardrec.horse >= 3 then
        new_cards { cardrec with horse = cardrec.horse - 3 }
      else if cardrec.tank >= 3 then
        new_cards { cardrec with tank = cardrec.tank - 3 }
      else if
        cardrec.soldier >= 1 && cardrec.horse >= 1 && cardrec.tank >= 1
      then
        new_cards
          {
            soldier = cardrec.soldier - 1;
            horse = cardrec.horse - 1;
            tank = cardrec.tank - 1;
          }
      else raise InvalidTrade

let string_of_card = function
  | Soldier -> "Soldier"
  | Horse -> "Horse"
  | Tank -> "Tank"

let card_to_json card = `String (string_of_card card)

let card_from_json str =
  match str with
  | "Soldier" -> Soldier
  | "Horse" -> Horse
  | "Tank" -> Tank
  | _ -> Soldier
