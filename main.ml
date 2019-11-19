open Graphics
open Unix


type keey = Up | Down | Left | Right | Space

type inpt = keey option

(** [unwrap_state ()] is the current game state. *)
let unwrap_state () = !Game.state

(** [state] is the current game state. *)
let state = unwrap_state ()

(** [check_still_alive ()] continues the input loop if the player still has at 
    least one life left. *)
let rec check_still_alive () = 
  if Game.get_lives (unwrap_state ()) = 0 
  then close_graph ()
  else check_key_pressed (wait_next_event [Key_pressed])

(** [check_key_pressed press] updates the game state on each player input. *)
and check_key_pressed press = 
  match press.key with
  | 'i' -> print_endline "up"; Game.update "up"; check_still_alive ()
  | 'j' -> print_endline "left"; Game.update "left"; check_still_alive ()
  | 'k' -> print_endline "down"; Game.update "down"; check_still_alive ()
  | 'l' -> print_endline "right"; Game.update "right"; check_still_alive ()
  | 'q' -> print_endline "You quit the game :("; close_graph ()
  | ' ' -> print_endline "Paused. Press any key to resume.";
    wait_next_event [Key_pressed]; ()
  | _ -> print_endline "bad"; Game.update "beat"; check_still_alive ()

(** [call_update num] updates the game state for a beat. *)
let call_update num = 
  Game.update "beat"

(** [play_game song_file num_players] initializes the game with the appropriate 
    values given by the player and [song_file] *)
let rec play_game song_file num_players =
  let song = Song.from_json (Yojson.Basic.from_file song_file) in
  Game.init_state num_players (Song.bpm song);
  Graphic.init_graphics "" state;
  let its = {it_interval = (Game.beats_per_sec ());
             it_value = 1.0} in
  setitimer ITIMER_REAL its; 
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (call_update));
  check_key_pressed (wait_next_event [Key_pressed])

(** [song_selection_loop ()] prompts the player to select the song they wish to 
    play and returns the name of that song file.*)
let rec song_selection_loop () = 
  print_endline "Please enter the number of the song you wish to play\n";
  print_endline "1: Song 1, Difficulty: Easy";
  print_endline "2: Song 2, Difficulty: Medium";
  print_endline "3: Song 3, Difficulty: Hard";
  print_string  "> ";
  match read_line () with
  | "1" -> "coughSyrup.json"
  | "2" -> "test_song.json" 
  | "3" -> "test_song_fast.json"
  | _ -> print_endline "Please enter a valid song number";
    song_selection_loop ()


(** [num_player_selection_loop ()] prompts the player to select the number of 
    players they wish to play with and returns the number. *)
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
