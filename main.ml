open Graphics
open Unix


type keey = Up | Down | Left | Right | Space

type inpt = keey option

let leaderboard = []

(** [unwrap_state ()] is the current game state. *)
let unwrap_state () = !Game.state

(** [state] is the current game state. *)
let state = unwrap_state ()

(** [check_still_alive ()] continues the input loop if the player still has at 
    least one life left. *)
let rec check_still_alive () = 
  if (Game.get_lives ()) = 0 
  then close_graph ()
  else check_key_pressed (wait_next_event [Key_pressed])

(** [check_key_pressed press] updates the game state on each player input. *)
and check_key_pressed press = 
  match press.key with
  | 'i' -> print_endline "up"; Game.update "up"; check_still_alive ()
  | 'j' -> print_endline "left"; Game.update "left"; check_still_alive ()
  | 'k' -> print_endline "down"; Game.update "down"; check_still_alive ()
  | 'l' -> print_endline "right"; Game.update "right"; check_still_alive ()
  | 'q' -> print_endline "You quit the game :(";  Game.update "pause";
    Graphic.quit(); 
    if (wait_next_event[Key_pressed]).key = 'q' then close_graph ()
    else Game.update "resume"; check_still_alive ()
  | ' ' -> print_endline "Paused. Press any key to resume."; Game.update "pause"; 
    Graphic.pause ();
    (wait_next_event [Key_pressed]); Game.update "resume"; check_still_alive ()
  | _ -> print_endline "bad"; Game.update ""; check_still_alive ()

let set_timer () = 
  let its = {it_interval = (Game.speed ());
             it_value = (Game.speed ())} in
  setitimer ITIMER_REAL its; ()

(** [call_update num] updates the game state for a beat. *)
let call_update num = 
  set_timer ();
  Game.update "beat"

(** [play_game song_file num_players] initializes the game with the appropriate 
    values given by the player and [song_file] *)
let rec play_game song_file num_players =
  let song = Song.from_json (Yojson.Basic.from_file song_file) in
  Game.init_state num_players (Song.bpm song);
  Graphic.init_graphics "" num_players;
  set_timer ();
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (call_update));
  check_key_pressed (wait_next_event [Key_pressed])

let click_location click = 
  (click.mouse_x,click.mouse_y)

let button_clicked x1 x2 y1 y2 click = 
  match click_location click with 
  | (x,y) -> if y < y2 && y > y1 && x < x2 && x > x1 then true else false

let rec start_window s = 
  match Graphic.start_window s with
  | (x,y) -> 
    let click = (wait_next_event [Button_down]) in 
    let x1 = x in let x2 = x + 100 in let y1 = y in let y2 = y + 75 in
    if button_clicked x1 x2 y1 y2 click 
    then ()
    else start_window s

let rec player_selection s =
  match Graphic.player_selection s with
  | (b1, b2) ->  
    let click = (wait_next_event [Button_down]) in 
    match b1 with 
    | (b1x,b1y) -> let b1x1 = b1x in let b1x2 = b1x + 100 in 
      let b1y1 = b1y in let b1y2 = b1y + 75 in 
      if button_clicked b1x1 b1x2 b1y1 b1y2 click 
      then 1
      else match b2 with 
        | (b2x,b2y) -> let b2x1 = b2x in let b2x2 = b2x + 100 in 
          let b2y1 = b2y in let b2y2 = b2y + 75 in 
          if button_clicked b2x1 b2x2 b2y1 b2y2 click
          then 2
          else player_selection s

let rec level_selection s = 
  match Graphic.level_selection s with
  | (b1, b2, b3, b4) ->  
    let click = (wait_next_event [Button_down]) in 
    match b1 with 
    | (b1x,b1y) -> let b1x1 = b1x in let b1x2 = b1x + 100 in 
      let b1y1 = b1y in let b1y2 = b1y + 75 in 
      if button_clicked b1x1 b1x2 b1y1 b1y2 click 
      then "coughSyrup.json"
      else match b2 with 
        | (b2x,b2y) -> let b2x1 = b2x in let b2x2 = b2x + 100 in 
          let b2y1 = b2y in let b2y2 = b2y + 75 in 
          if button_clicked b2x1 b2x2 b2y1 b2y2 click
          then "test_song.json"
          else match b3 with 
            | (b3x,b3y) -> let b3x1 = b3x in let b3x2 = b3x + 100 in 
              let b3y1 = b3y in let b3y2 = b3y + 75 in 
              if button_clicked b3x1 b3x2 b3y1 b3y2 click
              then "test_song_fast.json"
              else match b4 with 
                | (b4x,b4y) -> let b4x1 = b4x in let b4x2 = b4x + 100 in 
                  let b4y1 = b4y in let b4y2 = b4y + 75 in 
                  if button_clicked b4x1 b4x2 b4y1 b4y2 click
                  then "test_song_fast.json"
                  else level_selection s

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
  (* ANSITerminal.(print_string [red]
                  "\n\nWelcome to Tap Tap Revenge.\n"); *)
  start_window "";
  (* let num_players = num_player_selection_loop () in
     let song = song_selection_loop () in *)
  let num_players = player_selection "" in 
  let song = level_selection "" in
  print_int num_players;
  print_endline song;
  play_game song num_players

let () = main ()
