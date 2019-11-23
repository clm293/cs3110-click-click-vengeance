open Graphics 

(** [to_list arr] converts the 2D array [arr] to a 2D list. *)
let to_list arr = 
  let rec helper list = 
    match list with 
    | [] -> []
    | [[||]] -> [[]]
    | h :: t -> Array.to_list h :: (helper t) in
  helper (Array.to_list arr)

(** [of_list list] converts the 2D list [list] to a 2D array. *)
let of_list list = 
  let rec helper list = 
    match list with 
    | [] -> []
    | h :: t -> Array.of_list h :: (helper t) in 
  Array.of_list (helper list)

(** [make3 arr] repeates the array [arr] 3 times. *)
let make3 arr =
  Array.make 3 arr

(** [create_right_arrow_matrix c1 c2] creates the matrix for a right arrow 
    with arrow color c1 and background color c2. *)
let create_right_arrow_matrix c1 c2 = 
  let empty = Array.make 75 c2 in 
  let emptysec = Array.make 21 empty in
  let line8  = Array.append (Array.append (Array.make 51 c2) (Array.make 3 c1)) 
      (Array.make 21 c2) in
  let line9  = Array.append (Array.append (Array.make 51 c2) (Array.make 6 c1)) 
      (Array.make 18 c2) in
  let line10 = Array.append (Array.append (Array.make 54 c2) (Array.make 6 c1)) 
      (Array.make 15 c2) in
  let line11 = Array.append (Array.append (Array.make 57 c2) (Array.make 6 c1)) 
      (Array.make 12 c2) in
  let line12 = Array.append (Array.append (Array.make 60 c2) (Array.make 6 c1)) 
      (Array.make 9 c2) in
  let line13 = Array.append (Array.append (Array.make 6 c2) (Array.make 63 c1)) 
      (Array.make 6 c2) in
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

(** [create_left_arrow_matrix c1 c2] creates the matrix for a left arrow 
    with arrow color c1 and background color c2. *)
let create_left_arrow_matrix c1 c2 = 
  let rec helper list = 
    match list with 
    | [] -> []
    | h :: t -> List.rev h :: helper t in
  helper (to_list (create_right_arrow_matrix c1 c2)) |> of_list

(** [create_up_arrow_matrix c1 c2] creates the matrix for a up arrow 
    with arrow color c1 and background color c2. *)
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

(** [create_down_arrow_matrix c1 c2] creates the matrix for a down arrow 
    with arrow color c1 and background color c2. *)
let create_down_arrow_matrix c1 c2 = 
  create_up_arrow_matrix c1 c2 |> Array.to_list |> List.rev |> Array.of_list

(** [draw_background_1 c1 c2 c3 c4 score lives hotstreak] draws the background 
    in the single player mode. *)
let draw_background_1 c1 c2 c3 c4 score lives (hotstreak:bool) = 
  set_color black;
  fill_rect 0 0 600 640;
  set_color c1;
  fill_rect 20 95 115 525;
  fill_rect 250 20 115 75;
  set_color c2;
  fill_rect 135 95 115 525;
  fill_rect 365 20 115 75;
  set_color c3;
  fill_rect 250 95 115 525;
  fill_rect 20 20 115 75;
  set_color c4;
  fill_rect 365 95 115 525;
  fill_rect 135 20 115 75;
  set_color white;
  moveto 500 610;
  draw_string ("Score: " ^ (string_of_int score));
  moveto 500 580;
  draw_string ("Lives");
  moveto 500 560;
  draw_string  ("remaining: " ^ (string_of_int lives));
  moveto 500 530;
  if hotstreak then draw_string  ("HOTSTREAK!") else ();
  ()

(** [draw_background_2 c1 c2 c3 c4 score1 lives1 hotstreak1 
    score2 lives2 hotstreak2] draws the background in the double player mode. *)
let draw_background_2 c1 c2 c3 c4 score1 lives1 (hotstreak1:bool) 
    score2 lives2 (hotstreak2:bool) = 
  set_color black;
  fill_rect 0 0 1200 640;
  set_color c1;
  fill_rect 20 95 115 525;
  fill_rect 250 20 115 75;
  fill_rect 620 95 115 525;
  fill_rect 850 20 115 75;
  set_color c2;
  fill_rect 135 95 115 525;
  fill_rect 365 20 115 75;
  fill_rect 735 95 115 525;
  fill_rect 965 20 115 75;
  set_color c3;
  fill_rect 250 95 115 525;
  fill_rect 20 20 115 75;
  fill_rect 850 95 115 525;
  fill_rect 620 20 115 75;
  set_color c4;
  fill_rect 365 95 115 525;
  fill_rect 135 20 115 75;
  fill_rect 965 95 115 525;
  fill_rect 735 20 115 75;

  set_color white;

  moveto 500 610;
  draw_string "Player 1";
  moveto 500 580;
  draw_string ("Score: " ^ (string_of_int score1));
  moveto 500 550;
  draw_string ("Lives");
  moveto 500 530;
  draw_string  ("remaining: " ^ (string_of_int lives1));
  moveto 500 500;
  if hotstreak1 then draw_string  ("HOTSTREAK!") else ();

  moveto 1100 610;
  draw_string "Player 2";
  moveto 1100 580;
  draw_string ("Score: " ^ (string_of_int score2));
  moveto 1100 550;
  draw_string ("Lives");
  moveto 1100 530;
  draw_string  ("remaining: " ^ (string_of_int lives2));
  moveto 1100 500;
  if hotstreak2 then draw_string  ("HOTSTREAK!") else ();
  ()

(** [draw_left_arrow x y] draws the left arrow with at [x] [y]*)
let draw_left_arrow x y = 
  draw_image (make_image (create_left_arrow_matrix black transp)) x y;
  ()

(** [draw_down_arrow x y] draws the down arrow with at [x] [y]*)
let draw_down_arrow x y = 
  draw_image (make_image (create_down_arrow_matrix black transp)) x y;
  ()

(** [draw_up_arrow x y] draws the up arrow with at [x] [y]*)
let draw_up_arrow x y = 
  draw_image (make_image (create_up_arrow_matrix black transp)) x y;
  ()

(** [draw_right_arrow x y] draws the right arrow with at [x] [y]*)
let draw_right_arrow x y = 
  draw_image (make_image (create_right_arrow_matrix black transp)) x y;
  ()

let draw_logo s = 
  set_color white;
  fill_rect 125 250 350 350;
  set_color black;
  moveto 200 500;
  draw_string "Tap Tap Revenge";
  ()

let draw_button str x y bkg_color txt_color= 
  set_color bkg_color;
  fill_rect x y 100 75;

  set_color black;
  draw_image (make_image [|[|black|]|])x y;
  draw_image (make_image [|[|black|]|])(x+100) y;
  draw_image (make_image [|[|black|]|])x (y+75);
  draw_image (make_image [|[|black|]|])(x+100) (y+75);

  set_color black;
  draw_rect (x+2) (y+2) 96 71;

  set_color txt_color;
  match text_size str with
  |(x_text,y_text) -> let x_pos = x + (100-x_text)/2 in 
    let y_pos = y + (75-y_text)/2 in
    moveto x_pos y_pos;
    draw_string str;
    (x,y)

let start_window s =
  open_graph s;
  resize_window 600 640;
  set_color black;
  fill_rect 0 0 600 640;
  set_window_title "Tap Tap Revenge";
  draw_logo s;
  draw_button "Start" 250 150 magenta white

let player_selection st = 
  clear_graph ();
  set_color black;
  fill_rect 0 0 600 640;
  draw_logo st;
  set_color white;
  match text_size "Select a Playing Mode" with
  | (x,_) -> let x_pos = (600-x)/2 in
    moveto x_pos 300;
    draw_string "Select a Playing Mode";
    (draw_button "Single Player" (400/3) 150 cyan black, 
     draw_button "Double Plyer" (800/3 + 100) 150 magenta white)

let level_selection st =
  clear_graph ();
  set_color black;
  fill_rect 0 0 600 640;
  draw_logo st;
  moveto 200 300;
  set_color white;
  match text_size "Select a Level" with
  | (x,_) -> let x_pos = (600-x)/2 in
    moveto x_pos 300;
    draw_string "Select a Level";
    (draw_button "Easy" (300/4) 150 cyan black, 
     draw_button "Medium" (600/4 + 100) 150 green black, 
     draw_button "Hard" (900/4 + 200) 150 magenta white)

let init_graphics s num_players = 
  clear_graph ();
  match num_players with 
  | 1 -> begin
      resize_window 600 640;
      draw_background_1 magenta green cyan yellow 0 5 false;
      draw_left_arrow 40 545;
      draw_down_arrow 155 545;
      draw_up_arrow 270 545;
      draw_right_arrow 385 545;
      ()
    end
  | 2 -> begin 
      resize_window 1200 640;
      draw_background_2 magenta green cyan yellow 0 5 false 0 5 false;
      draw_left_arrow 40 545;
      draw_down_arrow 155 545;
      draw_up_arrow 270 545;
      draw_right_arrow 385 545;
      draw_left_arrow 640 545;
      draw_down_arrow 755 545;
      draw_up_arrow 870 545;
      draw_right_arrow 985 545;
    end
  | _ -> failwith "Invalid number of players"

let update_graphics_1 matrix score lives hs = 
  let rec draw_row row i j = 
    match row with 
    | [] -> ()
    | h :: t -> if h = None then draw_row t i (j+1) else begin
        match j with
        | 0 -> draw_left_arrow 40 (545-(75*i)); draw_row t i (j+1)
        | 1 -> draw_down_arrow 155 (545-(75*i)); draw_row t i (j+1)
        | 2 -> draw_up_arrow 270 (545-(75*i)); draw_row t i (j+1)
        | 3 -> draw_right_arrow 385 (545-(75*i)); draw_row t i (j+1)
        | _ -> ()
      end in
  let rec helper matrix i = 
    match matrix with 
    | [] -> ()
    | h :: t -> draw_row h i 0; helper t (i+1); in 
  clear_graph ();
  draw_background_1 magenta green cyan yellow score lives hs;
  helper matrix 0

let update_graphics_2 matrix1 score1 lives1 hs1 matrix2 score2 lives2 hs2 = 
  let rec draw_row_1 row i j = 
    match row with 
    | [] -> ()
    | h :: t -> if h = None then draw_row_1 t i (j+1) else begin
        match j with
        | 0 -> draw_left_arrow 40 (545-(75*i)); draw_row_1 t i (j+1)
        | 1 -> draw_down_arrow 155 (545-(75*i)); draw_row_1 t i (j+1)
        | 2 -> draw_up_arrow 270 (545-(75*i)); draw_row_1 t i (j+1)
        | 3 -> draw_right_arrow 385 (545-(75*i)); draw_row_1 t i (j+1)
        | _ -> ()
      end in
  let rec helper_1 matrix i = 
    match matrix with 
    | [] -> ()
    | h :: t -> draw_row_1 h i 0; helper_1 t (i+1); in 

  let rec draw_row_2 row i j = 
    match row with 
    | [] -> ()
    | h :: t -> if h = None then draw_row_2 t i (j+1) else begin
        match j with
        | 0 -> draw_left_arrow 640 (545-(75*i)); draw_row_2 t i (j+1)
        | 1 -> draw_down_arrow 755 (545-(75*i)); draw_row_2 t i (j+1)
        | 2 -> draw_up_arrow 870 (545-(75*i)); draw_row_2 t i (j+1)
        | 3 -> draw_right_arrow 985 (545-(75*i)); draw_row_2 t i (j+1)
        | _ -> ()
      end in
  let rec helper_2 matrix i = 
    match matrix with 
    | [] -> ()
    | h :: t -> draw_row_2 h i 0; helper_2 t (i+1); in 

  clear_graph ();
  draw_background_2 magenta green cyan yellow score1 lives1 hs1 
    score2 lives2 hs2;
  helper_1 matrix1 0;
  helper_2 matrix2 0;
  ()

let pause s = 
  set_color white;
  moveto 200 320;
  draw_string "Paused";
  moveto 200 300;
  draw_string "Press any key to resume.";
  ()

let quit s = 
  set_color white;
  moveto 200 320;
  draw_string "Quit?";
  moveto 200 300;
  draw_string "Press q to quit.";
  ()

let restart s = 
  open_graph s;
  resize_window 600 640;
  set_color black;
  fill_rect 0 0 600 640;
  set_window_title "Tap Tap Revenge";
  draw_logo s;
  moveto 200 300;
  (draw_button "Play Again" (400/3) 150 cyan black, 
   draw_button "Quit" (800/3 + 100) 150 magenta white)
