open OUnit2
open Game
open Main
open Level

(** Our approach to testing:
 * unit tests for functions and initializations
 * testing changes made to the mutable state behave as intended
 * Much of our testing was done through game play and running our graphics.
    The extent of our in-game testing, as well as trial game play with friends
    outside our team, makes us confident in the correctness of our system.
*)

type arrow = Left | Down | Up | Right | Health 

let lb = leaderboard := []; !leaderboard
let new_lb = update_leaderboard 1.0; !leaderboard

let rec update_x_times x inpt p = 
  if x > 0 then update inpt p; update_x_times (x-1) inpt p

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
]

let speed_update_test2 = [
  "speed is increased at beat 40" >:: (fun _ -> assert_equal (1.3*.1.3) (get_bpm ())
                                          ~printer:string_of_float);
]

let pause_update_tests = [
  "speed doesn't change" >:: (fun _ -> assert_equal 1.0 (speed ())
                                 ~printer:string_of_float);
  "score doesn't change p1" >:: (fun _ -> (assert_equal 0.0 (get_score 1)));
  "score doesn't change p2" >:: (fun _ -> (assert_equal 0.0 (get_score 2)));
  "paused" >:: (fun _ -> (assert_equal true (get_paused ())));
]

let hit_tests = [
  "score should +1 p1" >:: (fun _ -> assert_equal 1.0 (get_score 1)
                               ~printer:string_of_float);
  "score should +1 p2" >:: (fun _ -> assert_equal 1.0 (get_score 2)
                               ~printer:string_of_float);
]

let hotstreak_tests = [
  "score should +2 p1" >:: (fun _ -> assert_equal 2.0 (get_score 1) 
                               ~printer:string_of_float);
  "score should +2 p2" >:: (fun _ -> assert_equal 2.0 (get_score 2)
                               ~printer:string_of_float);
]

let suite_one = "test suite one player" >::: one_player_tests 
let suite_two = "test suite two player" >::: two_player_tests
let suite_update_beat = "test update beat" >::: beat_update_tests
let suite_pause = "test update pause" >::: pause_update_tests
let suite_speed1 = "test increase speed 1" >::: speed_update_test1
let suite_speed2 = "test increase speed 2" >::: speed_update_test2
let suite_hit = "test hit" >::: hit_tests
let suite_hotstreak = "test hotstreak" >::: hotstreak_tests

let _ = 
  init_state 1 60.0 60; 
  run_test_tt_main suite_one;
  print_endline "done single init";
  init_state 2 60.0 60;
  run_test_tt_main suite_two;
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
  set_state test_matrix1 2 (get_bpm ()) false (get_beat ());
  update "left" 1;
  update "left" 2;
  update "beat" 2;
  print_endline (string_of_float (get_score 1));
  update "left" 1;
  update "left" 2;
  set_state test_matrix1 2 (get_bpm ()) false (get_beat ());
  update "left" 1;
  update "left" 2;
  set_state test_matrix1 2 (get_bpm ()) false (get_beat ());
  update "left" 1;
  update "left" 2;
  set_state test_matrix1 2 (get_bpm ()) false (get_beat ());
  update "left" 1;
  update "left" 2;
  set_state test_matrix1 2 (get_bpm ()) false (get_beat ());
  update "left" 1;
  update "left" 2;
  set_state test_matrix1 2 (get_bpm ()) false (get_beat ());
  update "left" 1;
  update "left" 2;
  print_endline (string_of_float (get_score 1));
  set_state test_matrix1 2 (get_bpm ()) false (get_beat ());
  update "left" 1;
  update "left" 2;
  print_endline (string_of_float (get_score 1));
  set_state test_matrix1 2 (get_bpm ()) false (get_beat ());
  update "left" 1;
  update "left" 2;
  print_endline (string_of_float (get_score 1));
  run_test_tt_main suite_hotstreak;
  print_endline "done hotstreak";







