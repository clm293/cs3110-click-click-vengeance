open Graphics
open Unix

type keey = Up | Down | Left | Right | Space

type inpt = keey option

(** [check_win_string ()] is a string that says which player won for the game.*)
let check_win_string () = 
  if Game.get_score 1 > Game.get_score 2 then "PLAYER 1 WINS" 
  else if Game.get_score 1 < Game.get_score 2 then "PLAYER 2 WINS!" 
  else "BOTH PLAYERS TIED!"

(** [check_still_alive_1 np] checks if the player is still alive in a single
    player game. *)
let rec check_still_alive_1 num_players = 
  if Game.get_lives 1 <= 0
  then (Game.update "quit" num_players; restart "YOU LOSE!" num_players; ()) 
  else if Game.get_beat () >= Game.get_length ()
  then (Game.update "quit" num_players; restart "YOU WIN!" num_players; ())
  else check_key_pressed (wait_next_event [Key_pressed]) num_players

(** [check_still_alive_2 np] checks if both players are still alive in a 
    multiplayer game.*)
and check_still_alive_2 num_players = 
  if (Game.get_lives 1 <= 0 || (Game.get_lives 2) <= 0) 
  then (Game.update "quit" num_players; restart (check_win_string ())
          num_players; ())
  else check_key_pressed (wait_next_event [Key_pressed]) 2

(** [check_still_alive num_players] calls an end to the game if the conditions 
    in the game state signal to do so. Otherwise continues the game with 
    num_players. *)
and check_still_alive num_players = 
  if num_players = 2 then check_still_alive_2 num_players
  else check_still_alive_1 num_players

(** [check_key_one press num_players] checks if a key was pressed for a single
    player game and updates the game accordingly. *)
and check_key_one press num_players = 
  match press.key with
  | 'i' -> Game.update "up" 1; check_still_alive num_players
  | 'j' -> Game.update "left" 1; check_still_alive num_players
  | 'k' -> Game.update "down" 1; check_still_alive num_players
  | 'l' -> Game.update "right" 1; check_still_alive num_players
  | 'q' -> Game.update "quit" 1; Game.update "pause" 1; Graphic.quit(); 
    if (wait_next_event[Key_pressed]).key = 'q' 
    then (restart "YOU QUIT!" 1; ())
    else Game.update "resume" 1; check_still_alive num_players
  | ' ' -> Game.update "pause" 1; Graphic.pause ();
    let _ = (wait_next_event [Key_pressed]) in 
    Game.update "resume" 1; 
    check_still_alive num_players
  | _ -> Game.update "" 1; check_still_alive num_players

(** [check_key_two press num_players] checks if a key was pressed for a two
    player game and updates the game accordingly. *)
and check_key_two press num_players = 
  match press.key with
  | 'i' -> Game.update "up" 2; check_still_alive num_players
  | 'j' -> Game.update"left" 2; check_still_alive num_players
  | 'k' -> Game.update "down" 2; check_still_alive num_players
  | 'l' -> Game.update "right" 2; check_still_alive num_players
  | 'w' -> Game.update "up" 1; check_still_alive num_players
  | 'a' -> Game.update "left" 1; check_still_alive num_players
  | 's' -> Game.update "down" 1; check_still_alive num_players
  | 'd' -> Game.update "right" 1; check_still_alive num_players
  | 'q' -> Game.update "quit" 2; Game.update "quit" 2; Graphic.quit(); 
    if (wait_next_event [Key_pressed]).key = 'q' 
    then (restart "YOU QUIT!" 2; ())
    else Game.update "resume" 2; check_still_alive num_players
  | ' ' -> Game.update "pause" 1; Graphic.pause ();
    let _ = (wait_next_event [Key_pressed]) in 
    Game.update "resume" 1; 
    check_still_alive num_players
  | _ -> Game.update "" 1; check_still_alive num_players

(** [check_key_pressed press num_players] calls a function to update
    the game state on each player input. Then calls a function continue or end
    the game as indicated by the game state. *)
and check_key_pressed press num_players = 
  if num_players = 1 then check_key_one press num_players
  else check_key_two press num_players

(** [set_timer ()] uses the game speed to set the alarm that moves the game. *)
and set_timer () =
  let its = {it_interval = (Game.speed ());
             it_value = (Game.speed ())} in
  let _ = setitimer ITIMER_REAL its in ()

(** [call_update num] updates the game state for a beat. *)
and call_update num_players num = 
  if Game.get_beat () = Game.get_length ()
  then (if num_players = 1 then (restart "YOU WIN!" num_players; ()) else
          restart (check_win_string ()) num_players; ())
  else Game.update "beat" num_players; set_timer ()

(** [play_game mode num_players] initializes the game with the appropriate 
    values given by the player via [mode] and [num_players] *)
and play_game mode num_players =
  match mode with
  | "endless" -> 
    Game.init_state num_players 50.0 max_int;
    Graphic.init_graphics "" num_players;
    set_timer ();
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (call_update num_players));
    check_key_pressed (wait_next_event [Key_pressed]) num_players; ()
  | _ -> 
    let level = Level.from_json (Yojson.Basic.from_file mode) in
    Game.init_state num_players (Level.bpm level) (Level.length level);
    Graphic.init_graphics "" num_players;
    set_timer ();
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (call_update num_players));
    check_key_pressed (wait_next_event [Key_pressed]) num_players; ()

(** [restart s num_players] uses the state to call graphics updates and 
    continue the game environment *)
and restart s num_players = 
  Game.update_leaderboard (Game.final_score 1);
  if num_players = 2 then Game.update_leaderboard (Game.final_score 2);
  match Graphic.restart_window "" (!Game.leaderboard) with
  | "play again" -> main ()
  | "quit" -> close_graph ()
  | _ -> restart s num_players

(** [start_window s] responds to user inputs into the starting screen. *)
and start_window s = 
  match Graphic.start_window "" with 
  | "start" -> ()
  | _ -> start_window s

(** [player_selection s] responds to user inputs  
    to give the number of players. *)
and player_selection s =
  match Graphic.player_selection_window "" with
  | 1 -> 1
  | 2 -> 2
  | _ -> player_selection s

(** [level_selection s] responds to user inputs to give the level. *)
and level_selection s = 
  match Graphic.level_selection_window s with
  | "easy.json" -> "easy.json"
  | "med.json" -> "med.json"
  | "hard.json" -> "hard.json"
  | "endless" -> "endless"
  | _ -> level_selection s

(** [main ()] runs the game. *)
and main () =
  start_window "";
  let num_players = player_selection "" in 
  let level = level_selection "" in
  play_game level num_players

(* TESTING_LINES: if you are testing, comment out the next line. *)
let () = main ()  
