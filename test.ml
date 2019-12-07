open OUnit2
open Game
open Main
open Level

(** Our approach to testing:
 *unit tests for function
 *testing changes made to the mutable state behave as intended
*)

let init_tests = [

  "lives" >:: (fun _ -> 
      (assert_equal (get_lives ()) 5));
  "score" >:: (fun _ -> 
      (assert_equal (score ()) 0 ));
  "speed" >:: (fun _ -> 
      (assert_equal (speed ()) 60.0 ));
  "pause false" >:: (fun _ -> 
      (assert_equal (get_paused ()) false));

] 

let paused_tests = [
  "pause true" >:: (fun _ -> 
      (assert_equal (get_paused ()) true));
  "score doesn't change when paused" >:: (fun _ -> 
      (assert_equal (score ()) 0));
]

let suite_init = "test suite init" >::: (init_tests) 
let suite_pause = "test suite pause" >::: (paused_tests) 

let () = Game.init_state 1 60.0; run_test_tt_main suite_init;
  print_endline "done init";
  Game.update "paused";  run_test_tt_main suite_pause; print_endline "done paused"
