exception IllegalAttack

type t = {
  from : Board.country;
  towards : Board.country;
  player : string;
}

type dice_roll = {
  attack : int list;
  defend : int list;
  attack_num_lost : int;
  defend_num_lost : int;
}

let owns player (country : Board.country) = country.owner = player

let rec is_adjacent c = function
  | h :: t -> if h = c then true else is_adjacent c t
  | [] -> false

let create_attack b player from towards =
  let from = Board.country b from in
  if (not (owns player from)) || from.troops < 2 then
    raise IllegalAttack
  else
    let towards = Board.country b towards in
    if
      owns player towards
      || not (is_adjacent towards.name from.adjacent_countries)
    then raise IllegalAttack
    else { from; towards; player }

(* Helper function to generate a random number betwenn 1 and 6*)
let random_dice_gen () =
  Random.self_init ();
  Random.int 6 + 1

(* Helper function to return an int list whose length is dependent on
   the number of troops at disposal*)
let attack_dice_roll (country : Board.country) =
  let roll =
    match country.troops with
    | 1 -> raise IllegalAttack
    | 2 -> [ random_dice_gen () ]
    | 3 -> [ random_dice_gen (); random_dice_gen () ]
    | _ ->
        [ random_dice_gen (); random_dice_gen (); random_dice_gen () ]
  in
  List.sort compare roll |> List.rev

let defend_dice_roll (country : Board.country) =
  let roll =
    match country.troops with
    | 1 -> [ random_dice_gen () ]
    | _ -> [ random_dice_gen (); random_dice_gen () ]
  in
  List.sort compare roll |> List.rev

(* Helper function to decide the outcome of a fight *)
let rec decide_outcome att def =
  match (att, def) with
  | attack :: alst, defend :: dlst ->
      let a, d = decide_outcome alst dlst in
      if attack > defend then (a, d + 1) else (a + 1, d)
  | _, _ -> (0, 0)

let roll_dice attack =
  let attack_result = attack_dice_roll attack.from in
  let defend_result = defend_dice_roll attack.towards in
  let attack_lost, defend_lost =
    decide_outcome attack_result defend_result
  in
  {
    attack = attack_result;
    defend = defend_result;
    attack_num_lost = attack_lost;
    defend_num_lost = defend_lost;
  }
