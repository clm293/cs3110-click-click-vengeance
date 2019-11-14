open Graphics
open Unix


type keey = Up | Down | Left | Right | Space

type inpt = keey option

let rec call_update st key num = 
  beat (Game.update st key) 

and beat st = 
  let key = check_key_pressed () in
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (call_update st key))

and check_key_pressed () = 
  match (wait_next_event [Poll]).key with
  | 'i' -> print_endline "up"; "up"
  | 'j' -> print_endline "left"; "left"
  | 'k' -> print_endline "down"; "down"
  | 'l' -> print_endline "right"; "right"
  | _ -> print_endline "bad"; "bad"

let set_timer its = 
  let _ = setitimer ITIMER_REAL its in
  ()

let start_loop st = 
  print_endline "in test";
  let its = {it_interval = 0.5;
             it_value = 0.5} in
  beat st;
  set_timer its; 
  wait_next_event [Key_pressed]; 
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
