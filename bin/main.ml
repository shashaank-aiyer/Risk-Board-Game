open Risk.Board
open Risk.State
open Risk.Command

exception Quit

let handle_exception f (x, y, z) default =
  match f x y z with
  | exception _ ->
      print_endline "Invalid Command";
      default
  | v -> v

let print color s = ANSITerminal.print_string [ color ] s

let print_error s = print_endline ("\n" ^ s ^ "\n\n")

let print_status s = print_endline ("\n" ^ s ^ "\n\n")

let print_success s = print_endline ("\n" ^ s ^ "\n\n")

let print_input s = print_endline ("\n" ^ s ^ "\n\n")

let init_troops_given = [ 40; 35; 30; 25; 20 ]

let get_input default f =
  match Comms.read () with
  | exception End_of_file -> default
  | input ->
      print_endline input;
      f input

let build_list = function
  | h :: t -> h :: h :: t
  | [] -> []

let parse_players input =
  let names = String.split_on_char ' ' input in
  let rec make_troops given names =
    match (given, names) with
    | num :: t, [] -> [ num ]
    | _ :: remaining, [ h ] -> make_troops remaining []
    | _ :: remaining, _ :: t -> build_list (make_troops remaining t)
    | _, _ -> []
  in
  (names, make_troops init_troops_given names)

type info = string * string * int

let rec list_to_string lst =
  match lst with
  | h :: t -> h ^ ", " ^ list_to_string t
  | [] -> ""

let rec int_list_to_string lst =
  match lst with
  | h :: t -> string_of_int h ^ " " ^ int_list_to_string t
  | [] -> ""

let rec remove lst elem =
  match lst with
  | h :: t -> if h = elem then t else h :: remove t elem
  | [] -> []

let rec ask countries (player : string) : info * string list =
  match get_input "" (fun x -> x) |> parse with
  | Pick c ->
      if List.mem c countries then ((c, player, 1), remove countries c)
      else
        let _ =
          print_error
            "Invalid country inputted. Make sure to type the name \
             exactly as you see it."
        in
        ask countries player
  | _
  | (exception _) ->
      let _ = print_error "Invalid command. Type in a country again" in
      ask countries player

(* This method goes through and asks each player once *)
let rec ask_all countries players troops =
  if countries = [] then ([], [], troops)
  else
    match (players, troops) with
    | player :: players, troop :: troops ->
        print_endline
          ("What country would you like " ^ player ^ "\nChoices: "
          ^ list_to_string countries);
        let info, remaining_countries = ask countries player in
        let lst, remaining_countries, troops =
          ask_all remaining_countries players troops
        in
        (info :: lst, remaining_countries, (troop - 1) :: troops)
    | _, _ -> ([], countries, [])

(* This method goes through and asks each player until all countries
   have been picked *)
let rec first_init countries players troops =
  if countries = [] then ([], troops)
  else
    let info, countries, troops = ask_all countries players troops in
    let new_info, updated_troops =
      first_init countries players troops
    in
    (info @ new_info, updated_troops)

let rec update_country_from_info total_troops info country player troops
    =
  if troops < 0 || troops > total_troops then
    raise (Failure "Invalid number of troops");
  match info with
  | (c, p, t) :: rest ->
      if c = country then
        if p <> player then
          raise (Failure "Player does not own country")
        else (c, p, t + troops) :: rest
      else
        (c, p, t)
        :: update_country_from_info total_troops rest country player
             troops
  | [] -> raise (Failure "Country doesn't exist.")

let rec setup_place_troops info player troops =
  print_status
    ("Player " ^ player ^ " has " ^ string_of_int troops
   ^ " troops available.");
  if troops = 0 then info
  else
    match get_input "" (fun x -> x) |> parse with
    | Place (t, c) -> (
        match update_country_from_info troops info c player t with
        | v -> setup_place_troops v player (troops - t)
        | exception Failure s ->
            print_error s;
            setup_place_troops info player troops)
    | _
    | (exception _) ->
        print_error "Illegal command";
        setup_place_troops info player troops

let rec complete_init players troops info =
  match (players, troops) with
  | p :: ps, t :: ts ->
      setup_place_troops info p t |> complete_init ps ts
  | _, _ -> info

let make_board file_name =
  Yojson.Basic.from_file file_name |> Risk.Board.from_json

let rec set_up_players num =
  if num = 0 then []
  else
    let parse_player input =
      match Risk.Command.parse input with
      | Add p -> Risk.Player.make_player p :: set_up_players (num - 1)
      | _ ->
          print_error "Command not allowed";
          set_up_players num
      | exception Risk.Command.Malformed ->
          print_error "Command not valid";
          set_up_players num
    in
    print_endline "Who's playing";
    get_input [] parse_player

let rec get_num_players () =
  print_status "How many people are playing?";
  match get_input "" (fun x -> x) |> parse with
  | Num_Players n ->
      if n > 6 || n < 2 then
        let _ =
          print_error
            "Please insert a number of players between 2 and 6"
        in
        get_num_players ()
      else n
  | _
  | (exception _) ->
      let _ =
        print_error
          "Invalid command. Please use the form \"Player Amount num\""
      in
      get_num_players ()

let rec set_up_game board : Risk.State.t =
  let num_players = get_num_players () in
  let players = set_up_players num_players in
  let rec make_list length value =
    if length = 0 then [] else value :: make_list (length - 1) value
  in
  let troops =
    List.nth init_troops_given (num_players - 2)
    |> make_list num_players
  in
  let info, troops =
    first_init (countries board)
      (List.map Risk.Player.get_name players)
      troops
  in
  let info =
    complete_init (List.map Risk.Player.get_name players) troops info
  in
  let b = set_up board info in
  init_state b players

let mode_check st mode = current_mode st = mode

let country_to_string (country : country) =
  "The country " ^ country.name ^ " is in the continent "
  ^ country.continent ^ " with adjacent countries "
  ^ list_to_string country.adjacent_countries
  ^ ". "
  ^ string_of_int country.troops
  ^ " troops are placed by " ^ country.owner

let rec input_parse f st str =
  match parse str with
  | Attack (from, towards) when f st Risk.State.Attack ->
      Attack (from, towards)
  | Place (troops, place) when f st Risk.State.Placement ->
      Place (troops, place)
  | Fortify (from, towards, troops) when f st (Risk.State.Fortify true)
    ->
      Fortify (from, towards, troops)
  | Player_Info player ->
      let countries, continents = player_info player st in
      print_status
        ("Player " ^ player ^ " owns the countries: "
        ^ list_to_string countries);
      print_status
        ("Player " ^ player ^ " owns the continents: "
        ^ list_to_string continents);
      input_parse f st (get_input "" (fun x -> x))
  | Country_Info country ->
      let c = country_info country st in
      print_endline (country_to_string c);
      input_parse f st (get_input "" (fun x -> x))
  | command -> command

let card_to_string (card : Risk.Card.card) =
  match card with
  | Soldier -> "Soldier"
  | Horse -> "Horse"
  | Tank -> "Tank"

let rec cards_to_string = function
  | h :: t -> card_to_string h ^ ", " ^ cards_to_string t
  | [] -> ""

let placement_begin_state st =
  let cards = current_player_cards st in
  print_status
    (current_player_turn st ^ " has " ^ cards_to_string cards
   ^ "Would you like to trade? ");
  match get_input "" (fun x -> x) |> parse with
  | Trade -> add_new_troops true st
  | Exit -> raise Quit
  | _ -> add_new_troops false st

let rec placement_state st =
  print_status
    (current_player_turn st ^ " has "
    ^ (st |> remaining_troops |> string_of_int)
    ^ " troops to place.");
  if remaining_troops st = 0 then complete_turn st
  else
    match get_input "" (fun x -> x) |> input_parse mode_check st with
    | Place (troops, country) -> place_troops st country troops
    | Exit -> raise Quit
    | _ ->
        print_endline "Invalid command";
        st

let dice_roll_to_string (dr : Risk.Attack.dice_roll) =
  "\nAttacker dice: "
  ^ int_list_to_string dr.attack
  ^ "\nDefender dice: "
  ^ int_list_to_string dr.defend
  ^ "\nAttacker loses "
  ^ string_of_int dr.attack_num_lost
  ^ " troops." ^ "\nDefender loses "
  ^ string_of_int dr.defend_num_lost
  ^ " troops."

let rec attack_state st =
  print_status
    (current_player_turn st
   ^ " is attacking. Choose which country to attack from, which \
      country to attack ");
  match get_input "" (fun x -> x) |> input_parse mode_check st with
  | Attack (from, towards) ->
      (* attack can cause an exception *)
      let updated_st, dice = attack st from towards in
      print_endline (dice_roll_to_string dice);
      updated_st
  | Continue -> complete_turn st
  | Exit -> raise Quit
  | _ ->
      print_endline "Invalid command";
      st

let rec attack_won_state st =
  let from, towards = get_attack_won_info st in
  print_success
    ("Congrats! You won the attack. You can move "
    ^ string_of_int (get_country_troops (board st) from - 1)
    ^ " troops.");
  match get_input "" (fun x -> x) |> parse with
  | Move troops -> attack_won st troops
  | _ ->
      print_endline "Invalid command";
      st

let rec fortify_state st =
  print_status
    (current_player_turn st
   ^ " is fortifying. Choose which country to move from, which country \
      to move towards, and how many troops to move.");
  match get_input "" (fun x -> x) |> input_parse mode_check st with
  | Fortify (from, towards, troops) ->
      complete_turn (fortify st from towards troops)
  | Continue -> complete_turn st
  | Exit -> raise Quit
  | _ ->
      print_endline "Invalid command";
      st

let victory_state st =
  print_success
    (current_player_turn st ^ " has won the game. Congratulations!");
  raise Quit

let play_turn st =
  match current_mode st with
  | Placement_Begin -> placement_begin_state st
  | Placement -> placement_state st
  | Attack -> attack_state st
  | Attack_Won _ -> attack_won_state st
  | Fortify b -> fortify_state st
  | Victory winner -> victory_state st

let rec play_game st =
  st |> state_to_json |> Yojson.Basic.to_string |> Comms.send;
  match play_turn st with
  | st -> play_game st
  | exception IllegalState ->
      print_error "Illegal State Occured";
      play_game st
  | exception MissingLocation c ->
      print_error (c ^ "is not a valid location");
      play_game st
  | exception UnallowedMovement c ->
      print_error ("Not allowed to move to " ^ c);
      play_game st
  | exception UnallowedMovementAmount num ->
      print_error
        ("Not allowed to move  " ^ string_of_int num ^ " Troops");
      play_game st
  | exception Malformed ->
      print_error "Illegal Command given";
      play_game st
  | exception Risk.Player.UnavailableTroops num ->
      print_error "Invalid num troops";
      play_game st
  | exception Risk.Attack.IllegalAttack ->
      print_error "That attack is illegal to do";
      play_game st
  | exception Empty ->
      print_error "Please insert command";
      play_game st
  (* I couldn't think of a way to quit out other than raising an
     exception and then returning *)
  | exception Quit ->
      print_error "Quitting Game";
      st
  | exception Failure s ->
      print_error ("Command resulted in failure: " ^ s);
      play_game st

(* (fun _ -> print_endline "Error in input"; set_up_game board) *)
(* let complete_init info_lst troops = match info_lst, troops

   let set_up json (players, troops) = let empty_board = from_json json
   in let countries = countries empty_board in ()

   let play_game json = print_endline "Who's playing"; match read_line
   () with | exception End_of_file -> () | input -> input |>
   parse_players |> set_up json *)

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to RISK.\n";
  let _ =
    (* Yojson.Basic.from_file "risk_board_simple.json" |> from_json |>
       set_up_game |> play_game *)
    Yojson.Basic.from_file "start_state.json"
    |> state_from_json |> play_game
  in
  ();
  Comms.close ()

let () = main ()