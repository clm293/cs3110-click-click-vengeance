open Graphics
open Unix

type keey = Up | Down | Left | Right | Space

type inpt = keey option

(** [check_still_alive num_players] calls an end to the game if the conditions 
    in the game state signal to do so.  Else continues the game with num_players. *)
let rec check_still_alive num_players = 
  if num_players = 2 then begin
    if (Game.get_lives 1 <= 0 || (Game.get_lives 2) <= 0) 
    && (Game.get_score 1 > Game.get_score 2) 
    then (Game.update "pause" num_players; restart "PLAYER 1 WINS!" num_players; 
          ())
    else if (Game.get_lives 1 <= 0 || (Game.get_lives 2) <= 0) 
         && (Game.get_score 1 < Game.get_score 2) 
    then (Game.update "pause" num_players; restart "PLAYER 2 WINS!" num_players; 
          ())
    else if (Game.get_lives 1 <= 0 || (Game.get_lives 2) <= 0) 
         && (Game.get_score 1 = Game.get_score 2)
    then (Game.update "pause" num_players; restart "IT'S A TIE!" num_players; 
          ())
    else check_key_pressed (wait_next_event [Key_pressed]) 2
  end
  else begin
    if Game.get_lives 1 <= 0
    then (Game.update "pause" num_players; restart "YOU LOSE!" num_players; ()) 
    else if Game.get_beat () >= Game.get_length ()
    then (Game.update "pause" num_players; restart "YOU WIN!" num_players; ())
    else check_key_pressed (wait_next_event [Key_pressed]) num_players
  end

(** [check_key_one press num_players] checks if a key was pressed for a single
    player game and updates the game accordingly. *)
and check_key_one press num_players = 
  match press.key with
  | 'i' -> Game.update "up" 1; check_still_alive num_players
  | 'j' -> Game.update "left" 1; check_still_alive num_players
  | 'k' -> Game.update "down" 1; check_still_alive num_players
  | 'l' -> Game.update "right" 1; check_still_alive num_players
  | 'q' -> Game.update "quit" 1; Graphic.quit(); 
    if (wait_next_event[Key_pressed]).key = 'q' 
    then (restart "YOU QUIT!" num_players; ())
    else Game.update "resume" 1; check_still_alive num_players
  | ' ' -> Game.update "pause" 1; Graphic.pause ();
    (wait_next_event [Key_pressed]); Game.update "resume" 1; 
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
  | 'q' -> Game.update "pause" 2; Graphic.quit(); 
    if (wait_next_event [Key_pressed]).key = 'q' 
    then (restart "" num_players; ())
    else Game.update "resume" 2; check_still_alive num_players
  | ' ' -> Game.update "pause" 1; Graphic.pause ();
    (wait_next_event [Key_pressed]); Game.update "resume" 1; 
    check_still_alive num_players
  | _ -> Game.update "" 1; check_still_alive num_players

(** [check_key_pressed press num_players] calls a function to update
    the game state on each player input. Then calls a function continue or end
    the game as indicated by the game state. *)
and check_key_pressed press num_players= 
  if num_players = 1 then check_key_one press num_players
  else check_key_two press num_players

(** [set_timer ()] uses the game speed to set the alarm that moves the game. *)
and set_timer () =
  let its = {it_interval = (Game.speed ());
             it_value = (Game.speed ())} in
  setitimer ITIMER_REAL its; ()

(** [call_update num] updates the game state for a beat. *)
and call_update num_players num = 
  set_timer ();
  if Game.get_beat () = Game.get_length ()
  then (Game.update "quit" num_players;
        if num_players = 1 then restart "YOU WIN!" num_players else
          restart "BOTH PLAYERS WIN!" num_players)
  else Game.update "beat" num_players

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
    print_endline (string_of_int (Level.length level));
    Game.init_state num_players (Level.bpm level) (Level.length level);
    Graphic.init_graphics "" num_players;
    set_timer ();
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (call_update num_players));
    check_key_pressed (wait_next_event [Key_pressed]) num_players; ()

(** [click_location click] is the x and y location of a user's click. *)
and click_location click = 
  (click.mouse_x,click.mouse_y)

(** [button clicked x1 x2 y1 y2 click] is true if a user's click
     falls within the boudnaries of a box with bounds [x1], [x2], [y1], [y2] *)
and button_clicked x1 x2 y1 y2 click = 
  match click_location click with 
  | (x,y) -> if y < y2 && y > y1 && x < x2 && x > x1 then true else false

(** [click_play_again b1x b1y click] checks weather play again was clicked. *)
and click_play_again b1x b1y click = 
  let b1x1 = b1x in 
  let b1x2 = b1x + 100 in 
  let b1y1 = b1y in 
  let b1y2 = b1y + 75 in 
  button_clicked b1x1 b1x2 b1y1 b1y2 click

(** [click_quit b2x b2y click] checks weather quit was clicked. *)
and click_quit b2x b2y click = 
  let b2x1 = b2x in 
  let b2x2 = b2x + 100 in 
  let b2y1 = b2y in 
  let b2y2 = b2y + 75 in 
  button_clicked b2x1 b2x2 b2y1 b2y2 click

(** [restart s num_players] uses the state to call graphics updates and 
    continue the game environment *)
and restart s num_players = 
  Game.update_leaderboard (Game.get_score 1);
  if num_players = 2 then Game.update_leaderboard (Game.get_score 2);
  let rec helper s = 
    match Graphic.restart s !Game.leaderboard with
    | (play_again, quit) ->  
      let click = (wait_next_event [Button_down]) in 
      match play_again with 
      | (b1x,b1y) -> 
        if click_play_again b1x b1y click
        then main ()
        else match quit with 
          | (b2x,b2y) -> 
            if click_quit b2x b2y click
            then close_graph ()
            else helper s in
  helper s

(** [help s] calls the graphcis functions for the help screen. *)
and help s = 
  match Graphic.help s with 
  | (x,y) -> let click = wait_next_event [Button_down] in 
    let x1 = x in let x2 = x + 30 in let y1 = y in let y2 = y + 30 in
    if button_clicked x1 x2 y1 y2 click 
    then ()
    else help s

(** [click_start bx by click] checks weather start was clicked. *)
and click_start bx by click = 
  let bx1 = bx in 
  let bx2 = bx + 100 in 
  let by1 = by in 
  let by2 = by + 75 in 
  button_clicked bx1 bx2 by1 by2 click

(** [click_help_s hx hy click] checks weather help was clicked 
    on the start page. *)
and click_help_s hx hy click = 
  let hx1 = hx in 
  let hx2 = hx + 30 in 
  let hy1 = hy in 
  let hy2 = hy + 30 in 
  button_clicked hx1 hx2 hy1 hy2 click

(** [start_window s] responds to user inputs into the starting screen. *)
and start_window s = 
  match Graphic.start_window s with
  | (b, h) ->  
    let click = (wait_next_event [Button_down]) in 
    match b with 
    | (bx,by) -> 
      if click_start bx by click
      then ()
      else match h with 
        | (hx,hy) -> 
          if click_help_s hx hy click
          then (help s; start_window s)
          else start_window s

(** [click_single b1x b1y click] checks weather signle player was clicked. *)
and click_single b1x b1y click = 
  let b1x1 = b1x in 
  let b1x2 = b1x + 100 in 
  let b1y1 = b1y in 
  let b1y2 = b1y + 75 in 
  button_clicked b1x1 b1x2 b1y1 b1y2 click 

(** [click_double b2x b2y click] checks weather double player was clicked. *)
and click_double b2x b2y click = 
  let b2x1 = b2x in 
  let b2x2 = b2x + 100 in 
  let b2y1 = b2y in 
  let b2y2 = b2y + 75 in 
  button_clicked b2x1 b2x2 b2y1 b2y2 click

(** [click_help_p hx hy click] checks weather help was clicked 
    on the player selection page. *)
and click_help_p hx hy click = 
  let hx1 = hx in 
  let hx2 = hx + 30 in 
  let hy1 = hy in 
  let hy2 = hy + 30 in 
  button_clicked hx1 hx2 hy1 hy2 click

(** [player_selection s] responds to user inputs 
    to give the number of players. *)
and player_selection s =
  match Graphic.player_selection s with
  | (b1, b2, h) ->  
    let click = (wait_next_event [Button_down]) in 
    match b1 with 
    | (b1x,b1y) -> 
      if click_single b1x b1y click
      then 1
      else match b2 with 
        | (b2x,b2y) -> 
          if click_double b2x b2y click
          then 2
          else match h with 
            | (hx,hy) -> 
              if click_help_p hx hy click
              then (help s; player_selection s)
              else player_selection s

(** [click_easy b1x b1y click] checks weather easy was clicked. *)
and click_easy b1x b1y click = 
  let b1x1 = b1x in 
  let b1x2 = b1x + 100 in 
  let b1y1 = b1y in 
  let b1y2 = b1y + 75 in 
  button_clicked b1x1 b1x2 b1y1 b1y2 click 

(** [click_double b2x b2y click] checks weather medium was clicked. *)
and click_med b2x b2y click = 
  let b2x1 = b2x in 
  let b2x2 = b2x + 100 in 
  let b2y1 = b2y in 
  let b2y2 = b2y + 75 in 
  button_clicked b2x1 b2x2 b2y1 b2y2 click

(** [click_hard b3x b3y click] checks weather hard was clicked. *)
and click_hard b3x b3y click = 
  let b3x1 = b3x in 
  let b3x2 = b3x + 100 in 
  let b3y1 = b3y in 
  let b3y2 = b3y + 75 in 
  button_clicked b3x1 b3x2 b3y1 b3y2 click

(** [click_endless b4x b4y click] checks weather endless was clicked. *)
and click_endless b4x b4y click = 
  let b4x1 = b4x in 
  let b4x2 = b4x + 100 in 
  let b4y1 = b4y in 
  let b4y2 = b4y + 75 in 
  button_clicked b4x1 b4x2 b4y1 b4y2 click

(** [click_help_l hx hy click] checks weather help was clicked 
    on the level selection page. *)
and click_help_l hx hy click =
  let hx1 = hx in 
  let hx2 = hx + 30 in 
  let hy1 = hy in 
  let hy2 = hy + 30 in 
  button_clicked hx1 hx2 hy1 hy2 click

(** [level_selection s] responds to user inputs to give the level. *)
and level_selection s = 
  match Graphic.level_selection s with
  | (b1, b2, b3, b4, h) ->  
    let click = (wait_next_event [Button_down]) in 
    match b1 with 
    | (b1x,b1y) -> 
      if click_easy b1x b1y click then "easy.json"
      else match b2 with 
        | (b2x,b2y) -> 
          if click_med b2x b2y click then "med.json"
          else match b3 with 
            | (b3x,b3y) -> 
              if click_hard b3x b3y click then "hard.json"
              else match b4 with 
                | (b4x,b4y) -> 
                  if click_endless b4x b4y click then "endless"
                  else match h with 
                    | (hx,hy) -> 
                      if click_help_l hx hy click 
                      then (help s; level_selection s)
                      else level_selection s

(** [main ()] runs the game. *)
and main () =
  start_window "";
  let num_players = player_selection "" in 
  let level = level_selection "" in
  print_int num_players;
  print_endline level;
  play_game level num_players

(* let () = main ()  *)