open Graphics
open Unix


type keey = Up | Down | Left | Right | Space

type inpt = keey option

let print_hi num = print_endline "hi"

let rec call_update st num = 
  match read_key () with
  | 'i' -> print_endline "up"; beat (Game.update st "up") 
  | 'j' -> print_endline "left"; beat (Game.update st "left")
  | 'k' -> print_endline "down"; beat(Game.update st "down")
  | 'l' -> print_endline "right"; beat (Game.update st "right")
  | _ -> print_endline "bad"; beat st

and beat st = 
  let helper num = fun _ -> Graphic.update_graphics (Game.get_matrix st) in
  (* let helper num = Graphic.update_graphics (Game.get_matrix st) in *)
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (helper))

let start_loop st = 
  print_endline "in test";
  let its = {it_interval = 0.5;
             it_value = 0.5} in
  beat st;
  setitimer ITIMER_REAL its; wait_next_event [Key_pressed]; ()

let rec loop st status = 
  match status.key with
  | 'i' -> print_endline "up"; loop (Game.update st "up") (wait_next_event [Key_pressed])(*Game.update st "up"*)
  | 'j' -> print_endline "left"; loop (Game.update st "left") (wait_next_event [Key_pressed])(*Game.update st "left"*)
  | 'k' -> print_endline "down"; loop (Game.update st "down") (wait_next_event [Key_pressed])(*Game.update st "right"*)
  | 'l' -> print_endline "right"; loop (Game.update st "right") (wait_next_event [Key_pressed])(*Game.update st "down"*)
  | _ -> print_endline "bad"; loop st (wait_next_event [Key_pressed])

let rec play_game song_file num_players =
  let song = Song.from_json (Yojson.Basic.from_file song_file) in
  let game = Game.init_state num_players (Song.bpm song) in
  let status = Graphic.init_graphics "" game in
  start_loop game
(*loop game status*)

let rec song_selection_loop () = 
  print_endline "Please enter the number of the song you wish to play\n";
  print_endline "1: Song 1, Difficulty: Easy";
  print_endline "2: Song 2, Difficulty: Medium";
  print_endline "3: Song 3, Difficulty: Hard";
  print_string  "> ";
  match read_line () with
<<<<<<< HEAD
  | "1" -> "coughSyrup.json"
=======
  (* this file is currently hard-coded for sake of testing, because we have no 
     other song files created yet, will be data-driven later. *)
  | "1" -> "test_song.json" 
>>>>>>> 0e79c4f5aedc980d9144d2e66274d43332e58387
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
