open Graphics

type key = Up | Down | Left | Right | Space

type inpt = key option

(*let get_key_pressed = 
  let e = Graphics.wait_next_event [Key_pressed] in
  match e.key with
  | 'i' -> Some Up
  | 'j' -> Some Left
  | 'k' -> Some Down
  | 'l' -> Some Right
  | _ -> None

  let rec loop st = 
  match get_key_pressed with
  | Some Up -> Game.update st "up"
  | Some Left -> Game.update st "left"
  | Some Right -> Game.update st "right"
  | Some Down -> Game.update st "down"
  | Some Space -> failwith "pause"
  | None -> failwith "bad key")*)

let init_graphics s = 
  open_graph s;
  resize_window 500 500;
  set_color Graphics.black;
  fill_rect 0 0 500 500;
  set_color Graphics.magenta;
  fill_rect 20 20 115 460;
  set_color Graphics.yellow;
  fill_rect 135 20 115 460;
  set_color Graphics.green;
  fill_rect 250 20 115 460;
  set_color Graphics.cyan;
  fill_rect 365 20 115 460;
  set_line_width 5;
  set_color Graphics.black;
  draw_rect 30 30 440 75;
  wait_next_event []; ()


let play_game song_file num_players =
  let song = Song.from_json (Yojson.Basic.from_file song_file) in
  let game = Game.init_state num_players (Song.bpm song) in
  init_graphics ""

let rec song_selection_loop () = 
  print_endline "Please enter the number of the song you wish to play\n";
  print_endline "1: Song 1, Difficulty: Easy";
  print_endline "2: Song 2, Difficulty: Medium";
  print_endline "3: Song 3, Difficulty: Hard";
  print_string  "> ";
  match read_line () with
  | "1" -> "test_song.json" 
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
