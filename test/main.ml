open OUnit2
open Risk
open Board
open Command

(******************** Helper methods for printer and comparing expected
  with results ********************)

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let pp_int i = string_of_int i

let pp_string s = "\"" ^ s ^ "\""

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let pp_country (country : Board.country) =
  "{ name = " ^ country.name ^ "; continent = " ^ country.continent
  ^ "; adjacent_countries = "
  ^ pp_list pp_string country.adjacent_countries
  ^ "; troops = "
  ^ string_of_int country.troops
  ^ "; owner = " ^ country.owner ^ " }"

let pp_continent (continent : Board.continent) =
  "{ name = " ^ continent.name ^ "; countries = "
  ^ pp_list pp_string continent.countries
  ^ "; bonus = "
  ^ string_of_int continent.bonus
  ^ " }"

let pp_board b =
  "{ continents = "
  ^ pp_list pp_continent b.continents
  ^ "; countries = "
  ^ pp_list pp_country b.countries
  ^ " }"

(******************** Helper variables ********************)
let b_tester =
  from_json (Yojson.Basic.from_file "test/test_board_simple.json")

let b_init =
  from_json (Yojson.Basic.from_file "test/risk_board_simple.json")

let state_tester =
  State.state_from_json (Yojson.Basic.from_file "test/start_state.json")

let state_tester2 =
  State.state_from_json (Yojson.Basic.from_file "test/state2.json")

let state_tester3 =
  State.state_from_json (Yojson.Basic.from_file "test/state3.json")

let state_tester4 =
  State.state_from_json (Yojson.Basic.from_file "test/state4.json")

let state_tester5 =
  State.state_from_json (Yojson.Basic.from_file "test/state2 copy.json")

let state_tester6 =
  State.state_from_json (Yojson.Basic.from_file "test/state3 copy.json")

let test_info =
  [
    ("Brazil", "Arnav", 7);
    ("Venezuela", "Arnav", 6);
    ("Peru", "Aryan", 9);
    ("Argentina", "Aryan", 8);
    ("Indonesia", "Shank", 9);
    ("New Guinea", "Shank", 14);
    ("Western Australia", "Manvir", 12);
    ("Eastern Australia", "Manvir", 7);
  ]

(******************** Board test suite begins ********************)

let set_up_test name board l expected =
  name >:: fun _ ->
  assert_equal expected (set_up board l) ~printer:pp_board

let set_up_invalid_test name board l =
  name >:: fun _ ->
  assert_raises Risk.Board.MissingInfo (fun () -> set_up board l)

let set_up_tests =
  [
    set_up_invalid_test "Set up test - missing info" b_init [];
    set_up_test "Set up test - tester board from empty" b_init test_info
      b_tester;
  ]

let country_test name board c expected =
  name >:: fun _ ->
  assert_equal expected (country board c) ~printer:pp_country

let country_invalid_test name board c =
  name >:: fun _ ->
  assert_raises (Risk.Board.MissingLocation c) (fun () ->
      country board c)

let country_tests =
  [
    country_test "country test - Venezuela" b_tester "Venezuela"
      {
        name = "Venezuela";
        continent = "South America";
        adjacent_countries = [ "Brazil"; "Peru" ];
        troops = 6;
        owner = "Arnav";
      };
    country_test "country test - Indonesia" b_tester "Indonesia"
      {
        name = "Indonesia";
        continent = "Australia";
        adjacent_countries =
          [ "Brazil"; "New Guinea"; "Western Australia" ];
        troops = 9;
        owner = "Shank";
      };
    country_invalid_test "country invalid test - Egypt" b_tester "Egypt";
    country_invalid_test "country invalid test - Venezuela, empty board"
      Board.empty "Venezuela";
  ]

let countries_test name board expected =
  name >:: fun _ ->
  assert_equal expected (countries board) ~cmp:cmp_set_like_lists
    ~printer:(pp_list pp_string)

let countries_tests =
  [
    countries_test "countries test - empty board" Board.empty [];
    countries_test "countries test - tester board" b_init
      [
        "Western Australia";
        "Eastern Australia";
        "Indonesia";
        "New Guinea";
        "Argentina";
        "Brazil";
        "Venezuela";
        "Peru";
      ];
  ]

let continents_test name board expected =
  name >:: fun _ ->
  assert_equal expected (continents board) ~cmp:cmp_set_like_lists
    ~printer:(pp_list pp_string)

let continents_tests =
  [
    continents_test "continents test - empty board" Board.empty [];
    continents_test "continents test - tester board" b_tester
      [ "Australia"; "South America" ];
  ]

let countries_in_continent_test name board c expected =
  name >:: fun _ ->
  assert_equal expected
    (countries_in_continent board c)
    ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)

let countries_in_continent_invalid_test name board c =
  name >:: fun _ ->
  assert_raises (Risk.Board.MissingLocation c) (fun () ->
      countries_in_continent board c)

let countries_in_continent_tests =
  [
    countries_in_continent_test "count in cont test - Australia" b_init
      "Australia"
      [
        "Western Australia";
        "Eastern Australia";
        "New Guinea";
        "Indonesia";
      ];
    countries_in_continent_test "count in cont test - South America"
      b_tester "South America"
      [ "Brazil"; "Peru"; "Argentina"; "Venezuela" ];
    countries_in_continent_invalid_test
      "coun in cont invalid test - Africa" b_tester "Africa";
    countries_in_continent_invalid_test
      "coun in cont invalid test - Australia, empty board" Board.empty
      "Australia";
  ]

let adjacent_countries_test name board c expected =
  name >:: fun _ ->
  assert_equal expected
    (adjacent_countries board c)
    ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)

let adjacent_countries_invalid_test name board c =
  name >:: fun _ ->
  assert_raises (Risk.Board.MissingLocation c) (fun () ->
      adjacent_countries board c)

let adjacent_countries_tests =
  [
    adjacent_countries_test
      "adjacent_countries_test - Western Australia" b_init
      "Western Australia"
      [ "New Guinea"; "Indonesia"; "Eastern Australia" ];
    adjacent_countries_test "adjacent_countries_test - Brazil" b_tester
      "Brazil"
      [ "Venezuela"; "Indonesia"; "Peru"; "Argentina" ];
    adjacent_countries_invalid_test
      "adjacent_countries_invalid_test - Egypt" b_tester "Egypt";
    adjacent_countries_invalid_test
      "adjacent_countries_invalid_test - Western Australia, empty board"
      Board.empty "Western Australia";
  ]

let get_countries_owned_test name board p expected =
  name >:: fun _ ->
  assert_equal expected
    (get_countries_owned board p)
    ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)

let get_countries_owned_tests =
  [
    get_countries_owned_test "get_countries_owned_test - empty board"
      b_init "Aryan" [];
    get_countries_owned_test "get_countries_owned_test - valid player"
      b_tester "Aryan" [ "Peru"; "Argentina" ];
    get_countries_owned_test "get_countries_owned_test - invalid player"
      b_tester "Adam" [];
  ]

(* let get_continents_owned_test name board p expected = name >:: fun _
   -> assert_equal expected (get_continents_owned board p)
   ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)

   let get_continents_owned_tests = [ get_continents_owned_test
   "get_countries_owned_test - empty board" b_init "Aryan" [];
   get_continents_owned_test "get_countries_owned_test - valid player"
   b_tester "Aryan" [ "South America"]; get_continents_owned_test
   "get_countries_owned_test - invalid player" b_tester "Adam" []; ] *)

let get_continent_bonus_test name board c expected =
  name >:: fun _ ->
  assert_equal expected (get_continent_bonus board c) ~printer:pp_int

let get_continent_bonus_invalid_test name board c =
  name >:: fun _ ->
  assert_raises (Risk.Board.MissingLocation c) (fun () ->
      get_continent_bonus board c)

let get_continent_bonus_tests =
  [
    get_continent_bonus_test "get_continent_bonus_test - empty board"
      b_init "South America" 2;
    get_continent_bonus_test "get_continent_bonus_test - tester board"
      b_tester "Australia" 2;
    get_continent_bonus_invalid_test
      "get_continent_bonus_test - invalid continent" b_tester "Asia";
  ]

let get_country_troops_test name board c expected =
  name >:: fun _ ->
  assert_equal expected (get_country_troops board c) ~printer:pp_int

let get_country_troops_invalid_test name board c =
  name >:: fun _ ->
  assert_raises (Risk.Board.MissingLocation c) (fun () ->
      get_country_troops board c)

let get_country_troops_tests =
  [
    get_country_troops_test "get_country_troops_test - empty board"
      b_init "Peru" 0;
    get_country_troops_test "get_country_troops_test - tester board"
      b_tester "Western Australia" 12;
    get_country_troops_invalid_test
      "get_country_troops_test - invalid country" b_tester "Asia";
  ]

let get_country_owner_test name board c expected =
  name >:: fun _ ->
  assert_equal expected (get_country_owner board c) ~printer:pp_string

let get_country_owner_invalid_test name board c =
  name >:: fun _ ->
  assert_raises (Risk.Board.MissingLocation c) (fun () ->
      get_country_owner board c)

let get_country_owner_tests =
  [
    get_country_owner_test "get_country_owner_test - empty board" b_init
      "Peru" "";
    get_country_owner_test "get_country_owner_test - tester board"
      b_tester "Western Australia" "Manvir";
    get_country_owner_invalid_test
      "get_country_owner_test - invalid country" b_tester "Asia";
  ]

(* let cmp_country c1 c2 = c1.troops = c2.troops && c1.owner = c2.owner

   let rec cmp_countries clst1 clst2 = match (clst1, clst2) with | h1 ::
   t1, h2 :: t2 -> cmp_country h1 h2 && cmp_countries t1 t2 | [], [] ->
   true | _, _ -> false

   let cmp_boards b1 b2 = cmp_countries b1.countries b2.countries

   let print_country (c : country) = "\nCountry Name: " ^ c.name ^ "
   Troops: " ^ string_of_int c.troops ^ " Owner: " ^ c.owner ^ "\n"

   let rec print_countries = function | h :: t -> print_country h ^
   print_countries t | [] -> ""

   let print_board b = print_countries b.countries *)

let move_troops_test name board from towards amount expected =
  name >:: fun _ ->
  assert_equal expected
    (let new_board = move_troops board from towards amount in
     let from_troops = get_country_troops new_board from in
     let to_troops = get_country_troops new_board towards in
     (from_troops, to_troops))

let move_troops_invalid_test name board from towards amount exn =
  name >:: fun _ ->
  assert_raises exn (fun () -> move_troops board from towards amount)

let move_troops_tests =
  [
    move_troops_invalid_test "empty board invalid move" b_init "Brazil"
      "Venezuela" 0 (UnallowedMovementAmount 0);
    move_troops_test "Brazil to Venezuela 5" b_tester "Brazil"
      "Venezuela" 5
      ( get_country_troops b_tester "Brazil" - 5,
        get_country_troops b_tester "Venezuela" + 5 );
    move_troops_test "Western Australia to Eastern Australia 3" b_tester
      "Western Australia" "Eastern Australia" 3
      ( get_country_troops b_tester "Western Australia" - 3,
        get_country_troops b_tester "Eastern Australia" + 3 );
    move_troops_invalid_test "tester board invalid move - not adjacent"
      b_tester "Brazil" "Western Australia" 5
      (UnallowedMovement "Western Australia");
    move_troops_invalid_test
      "tester board invalid move - not enough troops" b_tester
      "Argentina" "Eastern Australia" 25 (UnallowedMovementAmount 25);
    move_troops_invalid_test
      "tester board invalid move - not enough troops and not adjacent"
      b_tester "Argentina" "Western Australia" 25
      (UnallowedMovementAmount 25);
  ]

(** commented out for compilation let place_troops_test name board
    amount c p expected = name >:: fun _ -> assert_equal expected
    (get_country_troops expected c + amount)

    let place_troops_tests =
    [
      place_troops_test "East Africa 3" expected 3 "Congo" "Arnav" ;
      place_troops_test "North Africa 2" expected "East Africa" "Indonesia" 3
        ( get_country_troops tester "Western Australia" - 3,
          get_country_troops tester "Indonesia" + 3 );
    ] *)

(*assert_equal ~cmp:cmp_set_like_lists expected (next_rooms adv room_id)
  ~printer:(pp_list pp_string) *)
(* assert_equal ~cmp:cmp_boards expected (move_troops board from towards
   amount) ~printer: print_board

   let move_troops_tests = [ move_troops_test "Basic Test" inital "East
   Africa" "Egypt" 5 expected; ] *)
let board_tests =
  List.flatten
    [
      set_up_tests;
      country_tests;
      countries_tests;
      continents_tests;
      countries_in_continent_tests;
      adjacent_countries_tests;
      get_countries_owned_tests;
      get_continent_bonus_tests;
      get_country_troops_tests;
      get_country_owner_tests;
      move_troops_tests;
    ]

(******************** Board test suite ends. Command test suite begins
  ********************)

let parse_test (name : string) (str : string) (expected_output : t) :
    test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (parse str)

let parse_test_exception name ex str =
  name >:: fun _ -> assert_raises ex (fun () -> parse str)

let parse_tests =
  [
    parse_test "Num Players" "Player Amount 3" (Num_Players 3);
    parse_test "Country info one word" "Country Info Egypt"
      (Country_Info "Egypt");
    parse_test "Country info two words" "Country Info South_Africa"
      (Country_Info "South Africa");
    parse_test "Fortify" "Fortify Egypt South_Africa 4"
      (Fortify ("Egypt", "South Africa", 4));
    parse_test "Place" "Place 3 South_Africa"
      (Place (3, "South Africa"));
    parse_test "Attacking 2 two word countries"
      "Attack East_Africa North_Africa"
      (Attack ("East Africa", "North Africa"));
    parse_test_exception "Malformed" Risk.Command.Malformed
      "Fotrify Egypt ";
  ]

let command_tests = List.flatten [ parse_tests ]

(******************** Command test suite ends. State test suite begins
  ********************)

let current_player_turn_test name state expected_output =
  name >:: fun _ ->
  assert_equal expected_output (State.current_player_turn state)

let current_player_cards_test name state expected_output =
  name >:: fun _ ->
  assert_equal expected_output (State.current_player_cards state)

let current_mode_test name state expected_output =
  name >:: fun _ ->
  assert_equal expected_output (State.current_mode state)

let add_new_troops_test name state expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    State.(add_new_troops true state |> remaining_troops)
    ~printer:string_of_int

let place_troops_test name state country troops expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (let board =
       State.place_troops state country troops |> State.board
     in
     Board.get_country_troops board country)
    ~printer:string_of_int

let attack_won_test name state towards troops expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (let board = State.attack_won state troops |> State.board in
     ( Board.get_country_troops board towards,
       Board.get_country_owner board towards ))

let fortify_test name state from towards troops expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (let board =
       State.fortify state from towards troops |> State.board
     in
     ( Board.get_country_troops board towards,
       Board.get_country_troops board from ))

let complete_turn_test name state expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (State.complete_turn state |> State.current_mode)

let complete_turn_fails_test name state ex =
  name >:: fun _ ->
  assert_raises ex (fun () -> State.complete_turn state)

let attack_test name state from towards =
  name >:: fun _ ->
  assert_equal true
    (let mode =
       let st, out = State.attack state from towards in
       State.current_mode st
     in
     mode = Attack || mode = Attack_Won (from, towards))

let state_tests =
  [
    current_player_turn_test "Current Player Test" state_tester "Arnav";
    current_player_cards_test "Current Player Cards Test" state_tester
      [ Soldier; Horse ];
    current_mode_test "Current Mode Test" state_tester Placement_Begin;
    add_new_troops_test "Add New Troops Test" state_tester 3;
    place_troops_test "Place Troops Test" state_tester2 "Brazil" 2 2;
    current_mode_test "Tester" state_tester3
      (Attack_Won ("Brazil", "Peru"));
    attack_won_test "Attack Won Test" state_tester3 "Peru" 2 (2, "Arnav");
    fortify_test "Fortify Test" state_tester4 "Brazil" "Peru" 2 (2, 1);
    complete_turn_test "Complete Turn Placement Begin" state_tester
      Placement;
    complete_turn_fails_test "Complete Turn Placement" state_tester2
      State.IllegalState;
    complete_turn_test "Complete Turn Placement Valid" state_tester5
      Attack;
    complete_turn_fails_test "Complete Turn Attack Won" state_tester3
      State.IllegalState;
    complete_turn_test "Complete Turn Fortify" state_tester4
      Placement_Begin;
    attack_test "Attack Test 1" state_tester6 "Brazil" "Peru";
  ]

let suite =
  "Test suite for project"
  >::: List.flatten [ board_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
