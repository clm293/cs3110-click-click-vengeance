open OUnit2
open Game
open Main
open Song

(** SOURCE: CS3110 a2 release code 
    [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2 



let j = Yojson.Basic.from_file "test_song.json"
let json_tests = [
  "bpm" >:: (fun _ -> 
      (assert_equal (bpm (from_json j)) 100 ));
  "song name" >:: (fun _ -> 
      (assert_equal (song_name (from_json j)) "test song" ));
  "difficulty" >:: (fun _ -> 
      (assert_equal (difficulty (from_json j)) Easy ));
  "music file" >:: (fun _ -> 
      (assert_equal (music_file (from_json j)) "test song file" ));
  "bad test" >:: (fun _ -> 
      (assert_equal 1 0  ))
]

let tests = [
  "test init state" >:: (fun _ -> 
      (assert_equal init_state init_state ))

] 


let suite = "test suite" >::: (List.flatten [json_tests; tests]) 

let _ = run_test_tt_main suite