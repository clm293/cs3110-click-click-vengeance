open Graphics
open Unix


type keey = Up | Down | Left | Right | Space

type inpt = keey option

let rec call_update st key num = 
  beat (Game.update st) 

and beat st = 
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (call_update st "up"))

and check_key_pressed press st = 
  match press.key with
  | 'i' -> print_endline "up"; nother_loop (Game.update_score st "up")
  | 'j' -> print_endline "left"; nother_loop (Game.update_score st "left")
  | 'k' -> print_endline "down"; nother_loop (Game.update_score st "down")
  | 'l' -> print_endline "right"; nother_loop (Game.update_score st "right")
  | 'q' -> print_endline "You quit the game:("; close_graph (); ()
  | ' ' -> print_endline "Paused. Press any key to resume.";
    wait_next_event [Key_pressed];()
  | _ -> print_endline "bad"; nother_loop (Game.update_score st "");

and set_timer its = 
  let _ = setitimer ITIMER_REAL its in
  ()

and  start_loop st = 
  print_endline "in test";
  let its = {it_interval = 0.5;
             it_value = 0.5} in
  set_timer its; 
  nother_loop st

and nother_loop st = 
  beat st;
  check_key_pressed (wait_next_event [Key_pressed]) st; (* need to get this st to be the current state *)
  ()

let rec play_game song_file num_players =
  let song = Song.from_json (Yojson.Basic.from_file song_file) in
  let game = Game.init_state num_players (Song.bpm song) in
  let _ = Graphic.init_graphics "" game in
  start_loop game

let rec song_selection_loop () = 
  print_endline "Please enter the number of the song you wish to play\n";
  print_endline "1: Song 1, Difficulty: Easy";
  print_endline "2: Song 2, Difficulty: Medium";
  print_endline "3: Song 3, Difficulty: Hard";
  print_string  "> ";
  match read_line () with
  | "1" -> "coughSyrup.json"
  | "2" -> "test_song.json" 
  | "3" -> "test_song.json"
  | _ -> print_endline "Please enter a valid song number";
    song_selection_loop ()

let rec num_player_selection_loop () =
  print_endline "Please enter the number of players you wish to play\n";
  print_string  "> ";
  match read_line () with
  | "1" -> 1
  | "2" -> 2
  | _ -> print_endline "Please enter a valid number of players";
    num_player_selection_loop ()

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Tap Tap Revenge.\n");
  let num_players = num_player_selection_loop () in
  let song = song_selection_loop () in
  play_game song num_players

let () = main ()
