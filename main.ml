open Graphics
open Unix

type keey = Up | Down | Left | Right | Space

type inpt = keey option

let leaderboard = []

(** [unwrap_state ()] is the current game state. *)
let unwrap_state () = !Game.state

(** [state] is the current game state. *)
let state () = unwrap_state ()

(** [check_still_alive ()] continues the input loop if the player still has at 
    least one life left. *)
let rec check_still_alive num = 
  if (Game.get_lives 1) = 0 
  then (print_endline "player 1 died"; Graphic.restart ""; ());
  if (Game.get_lives 2) = 0 
  then (print_endline "player 2 died"; Graphic.restart ""; ());
  if Game.get_beat () > Game.get_length () then print_endline "You won!"; 
  Graphic.restart ""; 
  check_key_pressed (wait_next_event [Key_pressed]) num

(** [check_key_pressed press] updates the game state on each player input. *)
and check_key_pressed press num= 
  if num = 1 then begin
    match press.key with
    | 'i' -> print_endline "up1"; Game.update "up" 1; check_still_alive num 
    | 'j' -> print_endline "left1"; Game.update "left" 1; check_still_alive num
    | 'k' -> print_endline "down1"; Game.update "down" 1; check_still_alive num
    | 'l' -> print_endline "right1"; Game.update "right" 1; check_still_alive num
    | 'q' -> print_endline "You quit the game :(";  Game.update "pause" 1;
      Graphic.quit(); 
      if (wait_next_event[Key_pressed]).key = 'q' then (Graphic.restart ""; ())
      else Game.update "resume" 1; check_still_alive (num)
    | ' ' -> print_endline "Paused. Press any key to resume."; Game.update "pause" 1; 
      Graphic.pause ();
      (wait_next_event [Key_pressed]); Game.update "resume" 1; check_still_alive num
    | _ -> print_endline "bad"; Game.update ""; check_still_alive num end
  else begin 
    match press.key with
    | 'w' -> print_endline "up1"; Game.update "up" 1; check_still_alive num
    | 'a' -> print_endline "left1"; Game.update "left" 1; check_still_alive num
    | 's' -> print_endline "down1"; Game.update "down" 1; check_still_alive num
    | 'd' -> print_endline "right1"; Game.update "right" 1; check_still_alive num
    | 'i' -> print_endline "up2"; Game.update "up" 2; check_still_alive num
    | 'j' -> print_endline "left2"; Game.update "left" 2; check_still_alive num
    | 'k' -> print_endline "down2"; Game.update "down" 2; check_still_alive num
    | 'l' -> print_endline "right2"; Game.update "right" 2; check_still_alive num
    | 'q' -> print_endline "You quit the game :(";  Game.update "pause" 1;
      Graphic.quit(); 
      if (wait_next_event[Key_pressed]).key = 'q' then (Graphic.restart ""; ())
      else Game.update "resume" 1; check_still_alive num
    | ' ' -> print_endline "Paused. Press any key to resume."; Game.update "pause" 1; 
      Graphic.pause ();
      (wait_next_event [Key_pressed]); Game.update "resume" 1; check_still_alive num
    | _ -> print_endline "bad"; Game.update "" 1; check_still_alive num
  end
let rec set_timer () =
  print_endline "in timer";
  let its = {it_interval = (Game.speed ());
             it_value = (Game.speed ())} in
  setitimer ITIMER_REAL its; ()

(** [call_update num] updates the game state for a beat. *)
and call_update num = 
  set_timer ();
  Game.update "beat" 1

(** [play_game mode num_players] initializes the game with the appropriate 
    values given by the player and [song_file] *)
and play_game mode num_players =
  let num = Game.get_num_players () in 
  if mode = "endless" then begin
    Game.init_state num_players 50.0 max_int;
    Graphic.init_graphics "" num_players;
    set_timer ();
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (call_update));
    check_key_pressed (wait_next_event [Key_pressed]) num; end
  else begin
    let song = Song.from_json (Yojson.Basic.from_file mode) in
    Game.init_state num_players (Song.bpm song) (Song.length song);
    Graphic.init_graphics "" num_players;
    set_timer ();
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (call_update));
    check_key_pressed (wait_next_event [Key_pressed]) num;
  end

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
                  then "endless"
                  else level_selection s

let main () =
  start_window "";
  let num_players = player_selection "" in 
  let song = level_selection "" in
  print_int num_players;
  print_endline song;
  play_game song num_players

let () = main ()
