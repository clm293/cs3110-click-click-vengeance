open Graphics


type keey = Up | Down | Left | Right | Space

type inpt = keey option

let to_list arr = 
  let rec helper list = 
    match list with 
    | [] -> []
    | [[||]] -> [[]]
    | h :: t -> Array.to_list h :: (helper t) in
  helper (Array.to_list arr)

let of_list list = 
  let rec helper list = 
    match list with 
    | [] -> []
    | h :: t -> Array.of_list h :: (helper t) in 
  Array.of_list (helper list)

let make3 arr =
  Array.make 3 arr

let create_right_arrow_matrix c1 c2 = 
  let empty = Array.make 75 c2 in 
  let emptysec = Array.make 21 empty in
  let line8  = Array.append (Array.append (Array.make 51 c2) (Array.make 3 c1)) (Array.make 21 c2) in
  let line9  = Array.append (Array.append (Array.make 51 c2) (Array.make 6 c1)) (Array.make 18 c2) in
  let line10 = Array.append (Array.append (Array.make 54 c2) (Array.make 6 c1)) (Array.make 15 c2) in
  let line11 = Array.append (Array.append (Array.make 57 c2) (Array.make 6 c1)) (Array.make 12 c2) in
  let line12 = Array.append (Array.append (Array.make 60 c2) (Array.make 6 c1)) (Array.make 9 c2) in
  let line13 = Array.append (Array.append (Array.make 6 c2) (Array.make 63 c1)) (Array.make 6 c2) in
  let middlesec = (make3 line8) 
                  |> Array.append (make3 line9) 
                  |> Array.append (make3 line10) 
                  |> Array.append (make3 line11) 
                  |> Array.append (make3 line12) 
                  |> Array.append (make3 line13) 
                  |> Array.append (make3 line12) 
                  |> Array.append (make3 line11) 
                  |> Array.append (make3 line10) 
                  |> Array.append (make3 line9) 
                  |> Array.append (make3 line8) in
  emptysec 
  |> Array.append middlesec 
  |> Array.append emptysec 

let create_left_arrow_matrix c1 c2 = 
  let rec helper list = 
    match list with 
    | [] -> []
    | h :: t -> List.rev h :: helper t in
  helper (to_list (create_right_arrow_matrix c1 c2)) |> of_list

let create_up_arrow_matrix c1 c2 = 
  let empty = Array.make 75 c2 in 
  let emptysec = Array.make 6 empty in 
  let line3 = Array.make 36 c2 
              |> Array.append (Array.make 3 c1) 
              |> Array.append (Array.make 36 c2) in 
  let line4 = Array.make 33 c2 
              |> Array.append (Array.make 9 c1) 
              |> Array.append (Array.make 33 c2) in 
  let line5 = Array.make 30 c2 
              |> Array.append (Array.make 15 c1) 
              |> Array.append (Array.make 30 c2) in
  let line6 = Array.make 27 c2 
              |> Array.append (Array.make 6 c1) 
              |> Array.append(Array.make 3 c2) 
              |> Array.append (Array.make 3 c1) 
              |> Array.append(Array.make 3 c2) 
              |> Array.append (Array.make 6 c1) 
              |> Array.append (Array.make 27 c2) in
  let line7 = Array.make 24 c2 
              |> Array.append (Array.make 6 c1) 
              |> Array.append(Array.make 6 c2) 
              |> Array.append (Array.make 3 c1) 
              |> Array.append(Array.make 6 c2) 
              |> Array.append (Array.make 6 c1) 
              |> Array.append (Array.make 24 c2) in
  let line8 = Array.make 21 c2 
              |> Array.append (Array.make 6 c1) 
              |> Array.append(Array.make 9 c2) 
              |> Array.append (Array.make 3 c1) 
              |> Array.append(Array.make 9 c2) 
              |> Array.append (Array.make 6 c1) 
              |> Array.append (Array.make 21 c2) in
  let middlesec1 = make3 line8 
                   |> Array.append (make3 line7) 
                   |> Array.append (make3 line6) 
                   |> Array.append (make3 line5) 
                   |> Array.append (make3 line4) 
                   |> Array.append (make3 line3) in 
  let middlesec2 = Array.make 45 line3 in 
  emptysec 
  |> Array.append middlesec2 
  |> Array.append middlesec1 
  |> Array.append emptysec

let create_down_arrow_matrix c1 c2 = 
  create_up_arrow_matrix c1 c2 |> Array.to_list |> List.rev |> Array.of_list

let init_graphics s = 
  open_graph s;
  resize_window 500 680;
  set_window_title "Tap Tap Revenge Game";
  set_color Graphics.black;
  fill_rect 0 0 500 680;
  set_color Graphics.magenta;
  fill_rect 20 20 115 640;
  set_color Graphics.yellow;
  fill_rect 135 20 115 640;
  set_color Graphics.green;
  fill_rect 250 20 115 640;
  set_color Graphics.cyan;
  fill_rect 365 20 115 640;
  set_line_width 5;
  set_color Graphics.black;
  draw_rect 30 30 440 75;

  draw_image (make_image (create_left_arrow_matrix Graphics.black Graphics.magenta)) 40 565;
  draw_image (make_image (create_down_arrow_matrix Graphics.black Graphics.yellow)) 155 565;
  draw_image (make_image (create_up_arrow_matrix Graphics.black Graphics.green)) 270 565;
  draw_image (make_image (create_right_arrow_matrix Graphics.black Graphics.cyan)) 395 565;

  wait_next_event [Key_pressed]

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
  let status = init_graphics "" in
  loop game status

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
