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
let rec check_still_alive num_players = 
  if num_players = 2 then begin
    if (Game.get_lives 1) <= 0 || (Game.get_lives 2) <= 0  
    then (Game.update "pause" num_players; restart "" num_players; ())
    else if (Game.get_beat () >= Game.get_length ()) then (restart "" num_players; print_endline "you won"; ())
    else check_key_pressed (wait_next_event [Key_pressed]) 2
  end
  else begin
    if (Game.get_lives 1) <= 0 
    then (Game.update "pause" num_players; restart "" num_players; ()) 
    else if (Game.get_beat () >= Game.get_length ()) then (restart "" num_players; print_endline "you won"; ())
    else check_key_pressed (wait_next_event [Key_pressed]) num_players
  end

(** [check_key_pressed press] updates the game state on each player input. *)
and check_key_pressed press num_players= 
  if num_players = 1 then
    match press.key with
    | 'i' -> print_endline "up"; Game.update "up" 1; check_still_alive num_players
    | 'j' -> print_endline "left"; Game.update "left" 1; check_still_alive num_players
    | 'k' -> print_endline "down"; Game.update "down" 1; check_still_alive num_players
    | 'l' -> print_endline "right"; Game.update "right" 1; check_still_alive num_players
    | 'q' -> print_endline "You quit the game :(";  Game.update "pause" 1;
      Graphic.quit(); 
      if (wait_next_event[Key_pressed]).key = 'q' then (restart "" num_players; ())
      else Game.update "resume" 1; check_still_alive num_players
    | ' ' -> print_endline "Paused. Press any key to resume."; Game.update "pause" 1; 
      Graphic.pause ();
      (wait_next_event [Key_pressed]); Game.update "resume" 1; check_still_alive num_players
    | _ -> print_endline "bad"; Game.update "" 1; check_still_alive num_players
  else 
    match press.key with
    | 'i' -> print_endline "up"; Game.update "up" 2; check_still_alive num_players
    | 'j' -> print_endline "left"; Game.update"left" 2; check_still_alive num_players
    | 'k' -> print_endline "down"; Game.update "down" 2; check_still_alive num_players
    | 'l' -> print_endline "right"; Game.update "right" 2; check_still_alive num_players
    | 'w' -> print_endline "up"; Game.update "up" 1; check_still_alive num_players
    | 'a' -> print_endline "left"; Game.update "left" 1; check_still_alive num_players
    | 's' -> print_endline "down"; Game.update "down" 1; check_still_alive num_players
    | 'd' -> print_endline "right"; Game.update "right" 1; check_still_alive num_players
    | 'q' -> print_endline "You quit the game :(";  Game.update "pause" 2;
      Graphic.quit(); 
      if (wait_next_event [Key_pressed]).key = 'q' then (restart "" num_players; ())
      else Game.update "resume" 2; check_still_alive num_players
    | ' ' -> print_endline "Paused. Press any key to resume."; Game.update "pause" 1; 
      Graphic.pause ();
      (wait_next_event [Key_pressed]); Game.update "resume" 1; check_still_alive num_players
    | _ -> print_endline "bad"; Game.update "" 1; check_still_alive num_players

and set_timer () =
  print_endline "in timer";
  let its = {it_interval = (Game.speed ());
             it_value = (Game.speed ())} in
  setitimer ITIMER_REAL its; ()

(** [call_update num] updates the game state for a beat. *)
and call_update num_players num = 
  set_timer ();
  Game.update "beat" num_players

(** [play_game mode num_players] initializes the game with the appropriate 
    values given by the player and [song_file] *)
and play_game mode num_players =
  begin match mode with
    | "endless" -> 
      Game.init_state num_players 50.0 max_int;
      Graphic.init_graphics "" num_players;
      set_timer ();
      Sys.set_signal Sys.sigalrm (Sys.Signal_handle (call_update num_players));
      check_key_pressed (wait_next_event [Key_pressed]) num_players;()
    | _ -> 
      let song = Song.from_json (Yojson.Basic.from_file mode) in
      Game.init_state num_players (Song.bpm song) (Song.length song);
      Graphic.init_graphics "" num_players;
      set_timer ();
      Sys.set_signal Sys.sigalrm (Sys.Signal_handle (call_update num_players));
      check_key_pressed (wait_next_event [Key_pressed]) num_players;()
  end

and click_location click = 
  (click.mouse_x,click.mouse_y)

and button_clicked x1 x2 y1 y2 click = 
  match click_location click with 
  | (x,y) -> if y < y2 && y > y1 && x < x2 && x > x1 then true else false

and restart s num_players = 
  Game.update_leaderboard (Game.get_score 1);
  if num_players = 2 then Game.update_leaderboard (Game.get_score 2);
  match Graphic.restart s !Game.leaderboard with
  | (play_again, quit) ->  
    let click = (wait_next_event [Button_down]) in 
    match play_again with 
    | (b1x,b1y) -> let b1x1 = b1x in let b1x2 = b1x + 100 in 
      let b1y1 = b1y in let b1y2 = b1y + 75 in 
      if button_clicked b1x1 b1x2 b1y1 b1y2 click 
      then main ()
      else match quit with 
        | (b2x,b2y) -> let b2x1 = b2x in let b2x2 = b2x + 100 in 
          let b2y1 = b2y in let b2y2 = b2y + 75 in 
          if button_clicked b2x1 b2x2 b2y1 b2y2 click
          then close_graph ()
          else restart s num_players

and lose s num_players = 
  Game.update_leaderboard (Game.get_score 1);
  if num_players = 2 then Game.update_leaderboard (Game.get_score 2);
  match Graphic.lose s with
  | (play_again, quit) ->  
    let click = (wait_next_event [Button_down]) in 
    match play_again with 
    | (b1x,b1y) -> let b1x1 = b1x in let b1x2 = b1x + 100 in 
      let b1y1 = b1y in let b1y2 = b1y + 75 in 
      if button_clicked b1x1 b1x2 b1y1 b1y2 click 
      then main ()
      else match quit with 
        | (b2x,b2y) -> let b2x1 = b2x in let b2x2 = b2x + 100 in 
          let b2y1 = b2y in let b2y2 = b2y + 75 in 
          if button_clicked b2x1 b2x2 b2y1 b2y2 click
          then close_graph ()
          else lose s num_players

and win s = 
  match Graphic.win s with
  | (play_again, quit) ->  
    let click = (wait_next_event [Button_down]) in 
    match play_again with 
    | (b1x,b1y) -> let b1x1 = b1x in let b1x2 = b1x + 100 in 
      let b1y1 = b1y in let b1y2 = b1y + 75 in 
      if button_clicked b1x1 b1x2 b1y1 b1y2 click 
      then main ()
      else match quit with 
        | (b2x,b2y) -> let b2x1 = b2x in let b2x2 = b2x + 100 in 
          let b2y1 = b2y in let b2y2 = b2y + 75 in 
          if button_clicked b2x1 b2x2 b2y1 b2y2 click
          then close_graph ()
          else win s

and help s = 
  match Graphic.help s with 
  | (x,y) -> let click = wait_next_event [Button_down] in 
    let x1 = x in let x2 = x + 30 in let y1 = y in let y2 = y + 30 in
    if button_clicked x1 x2 y1 y2 click 
    then ()
    else help s

and start_window s = 
  match Graphic.start_window s with
  | (b, h) ->  
    let click = (wait_next_event [Button_down]) in 
    match b with 
    | (bx,by) -> let bx1 = bx in let bx2 = bx + 100 in 
      let by1 = by in let by2 = by + 75 in 
      if button_clicked bx1 bx2 by1 by2 click
      then ()
      else match h with 
        | (hx,hy) -> let hx1 = hx in let hx2 = hx + 30 in 
          let hy1 = hy in let hy2 = hy + 30 in 
          if button_clicked hx1 hx2 hy1 hy2 click
          then (help s; start_window s)
          else start_window s

and player_selection s =
  match Graphic.player_selection s with
  | (b1, b2, h) ->  
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
          else match h with 
            | (hx,hy) -> let hx1 = hx in let hx2 = hx + 30 in 
              let hy1 = hy in let hy2 = hy + 30 in 
              if button_clicked hx1 hx2 hy1 hy2 click
              then (help s; player_selection s)
              else player_selection s

and level_selection s = 
  match Graphic.level_selection s with
  | (b1, b2, b3, b4, h) ->  
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
                  then "endless"
                  else match h with 
                    | (hx,hy) -> let hx1 = hx in let hx2 = hx + 30 in 
                      let hy1 = hy in let hy2 = hy + 30 in 
                      if button_clicked hx1 hx2 hy1 hy2 click
                      then (help s; level_selection s)
                      else level_selection s

and main () =
  start_window "";
  let num_players = player_selection "" in 
  let song = level_selection "" in
  print_int num_players;
  print_endline song;
  play_game song num_players

let () = main ()