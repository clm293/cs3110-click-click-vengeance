open OUnit2
open Game
open Main
open Level

(** Our approach to testing:
    We wrote unit tests to test game state initialization and game state updates.
    We wanted to make sure changing the mutable game state resulted in the intended behaviors.
    Since most of our functions in game.ml are private, we added testing functions in game.ml to allow for proper testing.
    Most of our testing was done through game play though, to properly simulate real playing conditions and all possible combinations of different conditions throughout the game.
    Our graphics cannot be tested through unit tests, so that testing was done primarily though playing.
    We also couldn't test our main.ml functions because they mostly rely on input/output from the graphics and terminal.
    We had friends play our game to maximize the extent of our testing.
*)

type arrow = Left | Down | Up | Right | Health 

let lb = leaderboard := []; !leaderboard
let new_lb = update_leaderboard 1.0; !leaderboard

let hot_streak_update x inpt p = 
  for x = 1 to x do  
    set_state test_matrix1 2 (get_bpm ()) false (get_beat ());
    update "left" 1;
    update "left" 2;
    update "beat" 2; 
  done

let one_player_tests = [
  "initial lives" >:: (fun _ -> (assert_equal 5 (get_lives 1)));
  "initial score" >:: (fun _ -> (assert_equal 0. (get_score 1)));
  "speed" >:: (fun _ -> (assert_equal 1.0 (speed ()) ));
  "pause false" >:: (fun _ -> (assert_equal false (get_paused ())));
  "empty leaderboard" >:: (fun _ -> (assert_equal [] lb));
  "leaderboard" >:: (fun _ -> (assert_equal [1.0] new_lb));
] 

let two_player_tests = [
  "initial lives p1" >:: (fun _ -> (assert_equal 5 (get_lives 1)));
  "initial lives p2" >:: (fun _ -> (assert_equal 5 (get_lives 2)));
  "initial score p1" >:: (fun _ -> (assert_equal 0. (get_score 1)));
  "initial score p2" >:: (fun _ -> (assert_equal 0. (get_score 2)));
  "speed" >:: (fun _ -> (assert_equal 1.0 (speed ())));
  "pause false" >:: (fun _ -> (assert_equal false (get_paused ())));
]

let beat_update_tests = [
  "speed doesn't change" >:: (fun _ -> assert_equal 1.0 (speed ()) 
                                 ~printer:string_of_float);
  "score doesn't change p1" >:: (fun _ -> (assert_equal 0.0 (get_score 1)));
  "score doesn't change p2" >:: (fun _ -> (assert_equal 0.0 (get_score 2)));
  "not paused" >:: (fun _ -> (assert_equal false (get_paused ())));
]

let speed_update_test1 = [
  "speed is increased at beat 20" >:: (fun _ -> assert_equal 1.3 (get_bpm ()) 
                                          ~printer:string_of_float);
  "base increase" >:: (fun _ -> assert_equal 1.1 (get_base_increase ())
                          ~printer:string_of_float)
]

let speed_update_test2 = [
  "speed is increased at beat 40" >:: (fun _ -> assert_equal (1.3*.1.3) 
                                          (get_bpm ())~printer:string_of_float);
]

let pause_update_tests = [
  "speed doesn't change" >:: (fun _ -> assert_equal 1.0 (speed ()) 
                                 ~printer:string_of_float);
  "score doesn't change p1" >:: (fun _ -> (assert_equal 0.0 (get_score 1)));
  "score doesn't change p2" >:: (fun _ -> (assert_equal 0.0 (get_score 2)));
  "paused" >:: (fun _ -> (assert_equal true (get_paused ())));
]

let hit_tests = [
  "score should +1 p1" >:: (fun _ -> assert_equal (string_of_float 1.2) (string_of_float (get_score 1)));
  "score should +1 p2" >:: (fun _ -> assert_equal (string_of_float 1.2) (string_of_float (get_score 2)));
]

let hotstreak_tests = [
  "score should +2 p1" >:: (fun _ -> assert_equal 14.4 (get_score 1) 
                               ~printer:string_of_float);
  "score should +2 p2" >:: (fun _ -> assert_equal 14.4 (get_score 2)
                               ~printer:string_of_float);
]

let miss_tests = [
  "score doesn't change p1" >:: (fun _ -> assert_equal 14.4 (get_score 1));
  "score updates p2" >:: (fun _ -> assert_equal 16.8 (get_score 2)
                             ~printer:string_of_float);
  "p1 lives -1" >:: (fun _ -> assert_equal 4 (get_lives 1));
]

let hotstreak_end_tests = [
  "p1 score increments by 1" >:: (fun _ -> assert_equal (string_of_float 15.6) (string_of_float (get_score 1)));
]

let add_life_tests = [
  "p1 lives +2" >:: (fun _ -> assert_equal 6 (get_lives 1)
                        ~printer:string_of_int);
  "p1 score doesn't change" >:: (fun _ -> assert_equal (string_of_float 15.6) (string_of_float (get_score 1)));
]

let suite_one = "test suite one player" >::: one_player_tests 
let suite_two = "test suite two player" >::: two_player_tests
let suite_update_beat = "test update beat" >::: beat_update_tests
let suite_pause = "test update pause" >::: pause_update_tests
let suite_speed1 = "test increase speed 1" >::: speed_update_test1
let suite_speed2 = "test increase speed 2" >::: speed_update_test2
let suite_hit = "test hit" >::: hit_tests
let suite_hotstreak = "test hotstreak" >::: hotstreak_tests
let suite_miss = "test miss" >::: miss_tests
let suite_hotstreak_end = "test hotstreak end" >::: hotstreak_end_tests
let suite_life = "test gaining life" >::: add_life_tests

let _ = 
  init_state 1 60.0 60; 
  run_test_tt_main suite_one;
  print_endline "done single init";
  init_state 2 60.0 60;
  run_test_tt_main suite_two;
  print_endline (string_of_float (get_base_increase ()));
  print_endline "done double init";
  update "beat" 1;
  run_test_tt_main suite_update_beat;
  print_endline "done beat";
  update "pause" 1;
  run_test_tt_main suite_pause;
  update "beat" 1;
  run_test_tt_main suite_pause;
  print_endline "done pause";
  set_state empty_matrix 2 1.0 false 19;
  update "beat" 1;
  run_test_tt_main suite_speed1;
  set_state empty_matrix 2 1.3 false 39;
  update "beat" 1;
  run_test_tt_main suite_speed2;
  print_endline "done speed";
  set_state test_matrix1 2 1.0 false 0;
  update "left" 1;
  update "left" 2;
  run_test_tt_main suite_hit;
  print_endline "done hits";
  hot_streak_update 11 "left" 2;
  run_test_tt_main suite_hotstreak;
  print_endline "done hotstreak";
  set_state test_matrix1 2 (get_bpm ()) false (get_beat ());
  update "right" 1;
  update "left" 2;
  update "beat" 2;
  run_test_tt_main suite_miss;
  print_endline "done miss";
  set_state test_matrix1 2 (get_bpm ()) false (get_beat ());
  update "left" 1;
  update "beat" 1;
  run_test_tt_main suite_hotstreak_end;
  print_endline "done hotstreak miss";
  set_state test_health_matrix 2 (get_bpm ()) false (get_beat ());
  update "left" 1;
  update "beat" 1;
  set_state test_health_matrix 2 (get_bpm ()) false (get_beat ());
  update "left" 1;
  update "beat" 1;
  run_test_tt_main suite_life;









