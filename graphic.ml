open Graphics 
open Images
open Png

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

(** [line8right c1 c2] draws the 8th line of the right arrow 
    with arrow color [c1] and background color [c2]. *)
let line8right c1 c2 = 
  (Array.make 21 c2)
  |> Array.append 
    (Array.append (Array.make 51 c2) (Array.make 3 c1))

(** [line9right c1 c2] draws the 9th line of the right arrow 
    with arrow color [c1] and background color [c2]. *)
let line9right c1 c2 = 
  (Array.make 18 c2)
  |> Array.append
    (Array.append (Array.make 51 c2) (Array.make 6 c1))

(** [line10right c1 c2] draws the 10th line of the right arrow 
    with arrow color [c1] and background color [c2]. *)
let line10right c1 c2 = 
  (Array.make 15 c2)
  |> Array.append 
    (Array.append (Array.make 54 c2) (Array.make 6 c1))

(** [line11right c1 c2] draws the 11th line of the right arrow 
    with arrow color [c1] and background color [c2]. *)
let line11right c1 c2 = 
  (Array.make 12 c2)
  |> Array.append 
    (Array.append (Array.make 57 c2) (Array.make 6 c1))

(** [line12right c1 c2] draws the 12th line of the right arrow 
    with arrow color [c1] and background color [c2]. *)
let line12right c1 c2 = 
  (Array.make 9 c2)
  |> Array.append 
    (Array.append (Array.make 60 c2) (Array.make 6 c1))

(** [line13right c1 c2] draws the 13th line of the right arrow 
    with arrow color [c1] and background color [c2]. *)
let line13right c1 c2 = 
  (Array.make 6 c2)
  |> Array.append 
    (Array.append (Array.make 6 c2) (Array.make 63 c1))

(** [create_right_arrow_matrix c1 c2] creates the matrix for a right arrow 
    with arrow color [c1] and background color [c2]. *)
let create_right_arrow_matrix c1 c2 = 
  let empty = Array.make 75 c2 in 
  let emptysec = Array.make 21 empty in
  let line8 = line8right c1 c2 in 
  let line9 = line9right c1 c2 in
  let line10 = line10right c1 c2 in
  let line11 = line11right c1 c2 in
  let line12 = line12right c1 c2 in
  let line13 = line13right c1 c2 in
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
  emptysec |> Array.append middlesec |> Array.append emptysec 

(** [create_left_arrow_matrix c1 c2] creates the matrix for a left arrow 
    with arrow color [c1] and background color [c2]. *)
let create_left_arrow_matrix c1 c2 = 
  let rec helper list = 
    match list with 
    | [] -> []
    | h :: t -> List.rev h :: helper t in
  helper (to_list (create_right_arrow_matrix c1 c2)) |> of_list

(** [line3up c1 c2] draws the 3rd line of the up arrow 
    with arrow color [c1] and background color [c2]. *)
let line3up c1 c2 = 
  Array.make 36 c2 
  |> Array.append (Array.make 3 c1) 
  |> Array.append (Array.make 36 c2)

(** [line4up c1 c2] draws the 4th line of the up arrow 
    with arrow color [c1] and background color [c2]. *)
let line4up c1 c2 = 
  Array.make 33 c2 
  |> Array.append (Array.make 9 c1) 
  |> Array.append (Array.make 33 c2)

(** [line5up c1 c2] draws the 5th line of the up arrow 
    with arrow color [c1] and background color [c2]. *)
let line5up c1 c2 = 
  Array.make 30 c2 
  |> Array.append (Array.make 15 c1) 
  |> Array.append (Array.make 30 c2)

(** [line6up c1 c2] draws the 6th line of the up arrow 
    with arrow color [c1] and background color [c2]. *)
let line6up c1 c2 = 
  Array.make 27 c2 
  |> Array.append (Array.make 6 c1) 
  |> Array.append(Array.make 3 c2) 
  |> Array.append (Array.make 3 c1) 
  |> Array.append(Array.make 3 c2) 
  |> Array.append (Array.make 6 c1) 
  |> Array.append (Array.make 27 c2)

(** [line7up c1 c2] draws the 7th line of the up arrow 
    with arrow color [c1] and background color [c2]. *)
let line7up c1 c2 = 
  Array.make 24 c2 
  |> Array.append (Array.make 6 c1) 
  |> Array.append(Array.make 6 c2) 
  |> Array.append (Array.make 3 c1) 
  |> Array.append(Array.make 6 c2) 
  |> Array.append (Array.make 6 c1) 
  |> Array.append (Array.make 24 c2)

(** [line8up c1 c2] draws the 8th line of the up arrow 
    with arrow color [c1] and background color [c2]. *)
let line8up c1 c2 = 
  Array.make 21 c2 
  |> Array.append (Array.make 6 c1) 
  |> Array.append(Array.make 9 c2) 
  |> Array.append (Array.make 3 c1) 
  |> Array.append(Array.make 9 c2) 
  |> Array.append (Array.make 6 c1) 
  |> Array.append (Array.make 21 c2)

(** [create_up_arrow_matrix c1 c2] creates the matrix for a up arrow 
    with arrow color [c1] and background color [c2]. *)
let create_up_arrow_matrix c1 c2 = 
  let empty = Array.make 75 c2 in 
  let emptysec = Array.make 6 empty in 
  let line3 = line3up c1 c2 in 
  let line4 = line4up c1 c2 in 
  let line5 = line5up c1 c2 in
  let line6 = line6up c1 c2 in
  let line7 = line7up c1 c2 in
  let line8 = line8up c1 c2 in
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
    with arrow color [c1] and background color [c2]. *)
let create_down_arrow_matrix c1 c2 = 
  create_up_arrow_matrix c1 c2 |> Array.to_list |> List.rev |> Array.of_list

(** [line3health c1 c2] draws the 3rd line of the health icon
    with color [c1] and background color [c2]. *)
let line3health c1 c2 = 
  Array.make 15 c2 
  |> Array.append (Array.make 12 c1)
  |> Array.append (Array.make 21 c2)
  |> Array.append (Array.make 12 c1)
  |> Array.append (Array.make 15 c2)

(** [line4health c1 c2] draws the 4th line of the health icon
    with color [c1] and background color [c2]. *)
let line4health c1 c2 = 
  Array.make 12 c2 
  |> Array.append (Array.make 18 c1)
  |> Array.append (Array.make 15 c2)
  |> Array.append (Array.make 18 c1)
  |> Array.append (Array.make 12 c2)

(** [line5health c1 c2] draws the 5th line of the health icon
    with color [c1] and background color [c2]. *)
let line5health c1 c2 = 
  Array.make 9 c2 
  |> Array.append (Array.make 24 c1)
  |> Array.append (Array.make 9 c2)
  |> Array.append (Array.make 24 c1)
  |> Array.append (Array.make 9 c2)

(** [line6health c1 c2] draws the 6th line of the health icon
    with color [c1] and background color [c2]. *)
let line6health c1 c2 = 
  Array.make 6 c2 
  |> Array.append (Array.make 30 c1)
  |> Array.append (Array.make 3 c2)
  |> Array.append (Array.make 30 c1)
  |> Array.append (Array.make 6 c2)

(** [topsec c1 c2] draws the top section of the health icon
    with color [c1] and background color [c2]. *)
let topsec c1 c2 = 
  let line3 = line3health c1 c2 in
  let line4 = line4health c1 c2 in
  let line5 = line5health c1 c2 in
  let line6 = line6health c1 c2 in
  (make3 line6) 
  |> Array.append (make3 line5) 
  |> Array.append (make3 line4) 
  |> Array.append (make3 line3)

(** [middlesec c1 c2] draws the middle section of the health icon
    with color [c1] and background color [c2]. *)
let middlesec c1 c2 = 
  let line = (Array.make 6 c2)
             |> Array.append 
               (Array.append (Array.make 6 c2) (Array.make 63 c1)) in 
  Array.make 21 line

(** [line14health c1 c2] draws the 14th line of the health icon
    with color [c1] and background color [c2]. *)
let line14health c1 c2 = 
  Array.make 9 c2 
  |> Array.append (Array.make 57 c1)
  |> Array.append (Array.make 9 c2)

(** [line15health c1 c2] draws the 15th line of the health icon
    with color [c1] and background color [c2]. *)
let line15health c1 c2 = 
  Array.make 12 c2 
  |> Array.append (Array.make 51 c1)
  |> Array.append (Array.make 12 c2)

(** [line16health c1 c2] draws the 16th line of the health icon
    with color [c1] and background color [c2]. *)
let line16health c1 c2 = 
  Array.make 15 c2 
  |> Array.append (Array.make 45 c1)
  |> Array.append (Array.make 15 c2)

(** [line17health c1 c2] draws the 17th line of the health icon
    with color [c1] and background color [c2]. *)
let line17health c1 c2 = 
  Array.make 18 c2 
  |> Array.append (Array.make 39 c1)
  |> Array.append (Array.make 18 c2)

(** [line18health c1 c2] draws the 18th line of the health icon
    with color [c1] and background color [c2]. *)
let line18health c1 c2 = 
  Array.make 21 c2 
  |> Array.append (Array.make 33 c1)
  |> Array.append (Array.make 21 c2)

(** [line19health c1 c2] draws the 19th line of the health icon
    with color [c1] and background color [c2]. *)
let line19health c1 c2 = 
  Array.make 24 c2 
  |> Array.append (Array.make 27 c1)
  |> Array.append (Array.make 24 c2)

(** [line20health c1 c2] draws the 20th line of the health icon
    with color [c1] and background color [c2]. *)
let line20health c1 c2 = 
  Array.make 27 c2 
  |> Array.append (Array.make 21 c1)
  |> Array.append (Array.make 27 c2)

(** [line21health c1 c2] draws the 21st line of the health icon
    with color [c1] and background color [c2]. *)
let line21health c1 c2 = 
  Array.make 30 c2 
  |> Array.append (Array.make 15 c1)
  |> Array.append (Array.make 30 c2)

(** [line22health c1 c2] draws the 22nd line of the health icon
    with color [c1] and background color [c2]. *)
let line22health c1 c2 = 
  Array.make 33 c2 
  |> Array.append (Array.make 9 c1)
  |> Array.append (Array.make 33 c2)

(** [line23health c1 c2] draws the 23rd line of the health icon
    with color [c1] and background color [c2]. *)
let line23health c1 c2 = 
  Array.make 36 c2 
  |> Array.append (Array.make 3 c1)
  |> Array.append (Array.make 36 c2)

(** [bottomsec c1 c2] draws the bottom section of the health icon
    with color [c1] and background color [c2]. *)
let bottomsec c1 c2 = 
  let line14 = line14health c1 c2 in
  let line15 = line15health c1 c2 in
  let line16 = line16health c1 c2 in
  let line17 = line17health c1 c2 in
  let line18 = line18health c1 c2 in
  let line19 = line19health c1 c2 in
  let line20 = line20health c1 c2 in
  let line21 = line21health c1 c2 in
  let line22 = line22health c1 c2 in
  let line23 = line23health c1 c2 in
  (make3 line23) 
  |> Array.append (make3 line22) 
  |> Array.append (make3 line21) 
  |> Array.append (make3 line20) 
  |> Array.append (make3 line19)
  |> Array.append (make3 line18)
  |> Array.append (make3 line17)
  |> Array.append (make3 line16)
  |> Array.append (make3 line15)
  |> Array.append (make3 line14)

(** [create_health c1 c2] creates the health icon
    with color [c1] and background color [c2]. *)
let create_health c1 c2 = 
  let empty = Array.make 75 c2 in
  let emptysec = Array.make 6 empty in 
  let topsec = topsec c1 c2 in 
  let middlesec = middlesec c1 c2 in
  let bottomsec = bottomsec c1 c2 in 
  emptysec 
  |> Array.append bottomsec 
  |> Array.append middlesec 
  |> Array.append topsec 
  |> Array.append emptysec

(** [draw_left_arrow x y] draws the left arrow with at ([x],[y]). *)
let draw_left_arrow x y = 
  draw_image (make_image (create_left_arrow_matrix black transp)) x y;
  ()

(** [draw_down_arrow x y] draws the down arrow with at [([x],[y])]. *)
let draw_down_arrow x y = 
  draw_image (make_image (create_down_arrow_matrix black transp)) x y;
  ()

(** [draw_up_arrow x y] draws the up arrow with at ([x],[y]). *)
let draw_up_arrow x y = 
  draw_image (make_image (create_up_arrow_matrix black transp)) x y;
  ()

(** [draw_right_arrow x y] draws the right arrow with at ([x],[y]). *)
let draw_right_arrow x y = 
  draw_image (make_image (create_right_arrow_matrix black transp)) x y;
  ()

(** [draw_health x y] draws the health symbol at given [x] [y]. *)
let draw_health x y = 
  draw_image (make_image (create_health red transp)) x y

(** [draw_heart x y] draws a heart for a life at the given [x] and [y]. *)
let draw_heart x y = 
  let img = Png.load_as_rgb24 "heart.png" [] in
  let g = Graphic_image.of_image img in
  Graphics.draw_image g x y;
  ()

(** [draw_background_1_colors c1 c2 c3 c4] draws the background 
    with the given color stripes in single player mode. *)
let draw_background_1_colors c1 c2 c3 c4 = 
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
  ()

(** [draw_background_1_scoreboard score lives hotstreak] draws the side panel 
    of the graphics screen that displays information about the player 
    in single player mode. *)
let draw_background_1_scoreboard score lives hotstreak = 
  set_color white;
  moveto 500 610;
  draw_string ("Score: " ^ (string_of_float score));
  moveto 500 580;
  if hotstreak then draw_string  ("HOTSTREAK!") else ();
  match lives with 
  | 0 -> ()
  | 1 -> draw_heart 500 550
  | 2 -> draw_heart 500 550; draw_heart 500 530
  | 3 -> draw_heart 500 550; draw_heart 500 530; draw_heart 500 510
  | 4 -> draw_heart 500 550; draw_heart 500 530; draw_heart 500 510; 
    draw_heart 500 490
  | 5 -> draw_heart 500 550; draw_heart 500 530; draw_heart 500 510; 
    draw_heart 500 490; draw_heart 500 470
  | _ -> draw_heart 500 550; draw_heart 500 530; draw_heart 500 510; 
    draw_heart 500 490; draw_heart 500 470; draw_heart 500 450;
    ()

(** [draw_background_1 c1 c2 c3 c4 score lives hotstreak] draws the background 
    in the single player mode. *)
let draw_background_1 c1 c2 c3 c4 (score:float) lives (hotstreak:bool) = 
  draw_background_1_colors c1 c2 c3 c4;
  draw_background_1_scoreboard score lives hotstreak;
  ()

(** [draw_background_2_colors c1 c2 c3 c4] draws the background 
    with the given color stripes in double player mode. *)
let draw_background_2_colors c1 c2 c3 c4 = 
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
  fill_rect 735 20 115 75; ()

(** [draw_lives_1 lives1] draws the lives for player 1 in double player mode. *)
let draw_lives_1 lives1 = 
  match lives1 with 
  | 1 -> draw_heart 500 520
  | 2 -> draw_heart 500 520; draw_heart 500 500
  | 3 -> draw_heart 500 520; draw_heart 500 500; draw_heart 500 480
  | 4 -> draw_heart 500 520; draw_heart 500 500; draw_heart 500 480; 
    draw_heart 500 460
  | 5 -> draw_heart 500 520; draw_heart 500 500; draw_heart 500 480; 
    draw_heart 500 460; draw_heart 500 440
  | _ -> draw_heart 500 520; draw_heart 500 500; draw_heart 500 480; 
    draw_heart 500 460; draw_heart 500 440; draw_heart 500 420;
    ()

(** [draw_lives_2 lives2] draws the lives for player 2 in double player mode. *)
let draw_lives_2 lives2 = 
  match lives2 with 
  | 1 -> draw_heart 1100 520
  | 2 -> draw_heart 1100 520; draw_heart 1100 500
  | 3 -> draw_heart 1100 520; draw_heart 1100 500; draw_heart 1100 480
  | 4 -> draw_heart 1100 520; draw_heart 1100 500; draw_heart 1100 480; 
    draw_heart 1100 460
  | 5 -> draw_heart 1100 520; draw_heart 1100 500; draw_heart 1100 480; 
    draw_heart 1100 460; draw_heart 1100 440
  | _ -> draw_heart 1100 520; draw_heart 1100 500; draw_heart 1100 480; 
    draw_heart 1100 460; draw_heart 1100 440; draw_heart 1100 420;
    ()

(** [draw_background_2 c1 c2 c3 c4 score1 lives1 hotstreak1 
    score2 lives2 hotstreak2] draws the background in the double player mode. *)
let draw_background_2 c1 c2 c3 c4 (score1:float) (lives1:int) (hotstreak1:bool) 
    (score2:float) (lives2:int) (hotstreak2:bool) = 
  set_color black;
  fill_rect 0 0 1200 640;
  draw_background_2_colors c1 c2 c3 c4;
  set_color white;
  moveto 500 610;
  draw_string "Player 1";
  moveto 500 580;
  draw_string ("Score: " ^ (string_of_float score1));
  moveto 500 550;
  if hotstreak1 then draw_string  ("HOTSTREAK!") else ();
  moveto 1100 610;
  draw_string "Player 2";
  moveto 1100 580;
  draw_string ("Score: " ^ (string_of_float score2));
  moveto 1100 550;
  if hotstreak2 then draw_string  ("HOTSTREAK!") else ();
  draw_lives_1 lives1;
  draw_lives_2 lives2;
  ()

(** [draw_help s] draws the help button on the bottom of the screen. *)
let draw_help s = 
  let img = Png.load_as_rgb24 "help.png" [] in
  let g = Graphic_image.of_image img in
  Graphics.draw_image g (0) (0);
  (0, 0)

(** [write_line str y] writes the line [str] in the center of the screen at 
    height [y]. *)
let write_line str y = 
  match text_size str with
  |(x_text,y_text) -> let x_pos = (size_x () - x_text)/2 in 
    let y_pos = y in
    moveto x_pos y_pos;
    draw_string str;
    ()

(** [help s] draws the help screen that appears with game instructions. *)
let help s = 
  set_color white;
  fill_rect 50 100 500 430;
  set_color magenta;
  write_line "Click Click Vengeance" 510;
  set_color black;
  write_line "The objective of Click CLick Vengence" 470;
  write_line "is to beat previous high scores: earn points by tapping" 440;
  write_line "the corresponding key when the arrows reach the bottom row." 410;
  write_line  ("Hits become more valuable as the game progresses and increases" 
               ^ "in difficulty, ") 380;
  write_line ("and extra points are awarded for leftover lives at the end of "
              ^ "the game.") 350;
  write_line ("There are 4 modes: easy, medium, hard, and endless -- play"
              ^ " alone or with a friend")  320;
  write_line ("In double player, Player 1 will use W (up), A (left), S (down),"
              ^ " D (right).") 290;
  write_line ("Player 2 will use I (up), J (left), K (down), L (right) -- IJKL"
              ^" are used in solo.") 260;
  write_line "In easy, medium, and hard, play until the game ends" 230;
  write_line "or you lose all your lives, whichever comes first." 200;
  write_line "In enless mode, play untl you lose all 5 lives." 170;
  write_line "Press space to pause, press 'q' to quit." 130;
  set_color cyan;
  write_line "GOOD LUCK!!!" 100;
  (0, 0)

(** [draw_logo s] draws the logo. *)
let draw_logo s = 
  let img = Png.load_as_rgb24 "logo.png" [] in
  let g = Graphic_image.of_image img in
  Graphics.draw_image g (125) (250);
  ()

(** [draw_button s] draws the button at ([x],[y]) 
    with [str] centered in the button. *)
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

(** [click_location click] is the x and y location of a user's click. *)
let click_location click = 
  (click.mouse_x,click.mouse_y)

(** [button_clicked x1 x2 y1 y2 click] is true if a user's click
     falls within the boudnaries of a box with bounds [x1], [x2], [y1], [y2] *)
let button_clicked x1 x2 y1 y2 click = 
  match click_location click with 
  | (x,y) -> if y < y2 && y > y1 && x < x2 && x > x1 then true else false

(** [clicked b1x b1y click] checks weather a button was clicked. *)
let clicked b1x b1y click = 
  let b1x1 = b1x in 
  let b1x2 = b1x + 100 in 
  let b1y1 = b1y in 
  let b1y2 = b1y + 75 in 
  button_clicked b1x1 b1x2 b1y1 b1y2 click

(** [help s] calls the graphcis functions for the help screen. *)
let rec help_window s = 
  match help s with 
  | (x,y) -> let click = wait_next_event [Button_down] in 
    let x1 = x in let x2 = x + 30 in let y1 = y in let y2 = y + 30 in
    if button_clicked x1 x2 y1 y2 click 
    then ()
    else help_window s

(** [start s] is the first window shown when the game starts. *)
let start s =
  open_graph s;
  resize_window 600 640;
  set_color black;
  fill_rect 0 0 600 640;
  set_window_title "Click Click Vengeance";
  draw_logo s;
  (draw_button "Start" 250 150 magenta black, draw_help s)

(** [start_window s] responds to user inputs into the starting screen. *)
let rec start_window s = 
  match start s with
  | (b, h) ->  
    let click = (wait_next_event [Button_down]) in 
    match b with 
    | (bx,by) -> 
      if clicked bx by click
      then "start"
      else match h with 
        | (hx,hy) -> 
          if clicked hx hy click
          then (help_window s; start_window s)
          else start_window s

(** [player_selection s] is the where the player(s) chooses 
    the number of players in the game. *)
let player_selection s = 
  clear_graph ();
  set_color black;
  fill_rect 0 0 600 640;
  draw_logo s;
  set_color white;
  match text_size "Select a Playing Mode" with
  | (x,_) -> let x_pos = (600-x)/2 in
    moveto x_pos 300;
    draw_string "Select a Playing Mode";
    (draw_button "Single Player" (400/3) 150 cyan black, 
     draw_button "Double Player" (800/3 + 100) 150 magenta black, 
     draw_help "")

(** [player_selection_window s] responds to user inputs  
    to give the number of players. *)
let rec player_selection_window s =
  match player_selection s with
  | (b1, b2, h) ->  
    let click = (wait_next_event [Button_down]) in 
    match b1 with 
    | (b1x,b1y) -> 
      if clicked b1x b1y click
      then 1
      else match b2 with 
        | (b2x,b2y) -> 
          if clicked b2x b2y click
          then 2
          else match h with 
            | (hx,hy) -> 
              if clicked hx hy click
              then (help_window s; player_selection_window s)
              else player_selection_window s

(** [level_selection s] is where the player(s) chooses the difficulty. *)
let level_selection s =
  clear_graph ();
  set_color black;
  fill_rect 0 0 600 640;
  draw_logo s;
  moveto 200 300;
  set_color white;
  match text_size "Select a Level" with
  | (x,_) -> let x_pos = (600-x)/2 in
    moveto x_pos 300;
    draw_string "Select a Level";
    (draw_button "Easy" (200/5) 150 yellow black, 
     draw_button "Medium" (400/5 + 100) 150 cyan black, 
     draw_button "Hard" (600/5 + 200) 150 green black,
     draw_button "Endless" (800/5 + 300) 150 magenta black, 
     draw_help "")

(** [level_selection_window s] responds to user inputs to give the level. *)
let rec level_selection_window s = 
  match level_selection s with
  | (b1, b2, b3, b4, h) ->  
    let click = (wait_next_event [Button_down]) in 
    match b1 with 
    | (b1x,b1y) -> 
      if clicked b1x b1y click then "easy.json"
      else match b2 with 
        | (b2x,b2y) -> 
          if clicked b2x b2y click then "med.json"
          else match b3 with 
            | (b3x,b3y) -> 
              if clicked b3x b3y click then "hard.json"
              else match b4 with 
                | (b4x,b4y) -> 
                  if clicked b4x b4y click then "endless"
                  else match h with 
                    | (hx,hy) -> 
                      if clicked hx hy click 
                      then (help_window s; level_selection_window s)
                      else level_selection_window s

(** [init_graphics s num_players] is where the first screen 
    when the game officially begins. *)
let init_graphics s num_players = 
  clear_graph ();
  match num_players with 
  | 1 -> resize_window 600 640;
    draw_background_1 magenta green cyan yellow 0.0 5 false;
    draw_left_arrow 40 545;
    draw_down_arrow 155 545;
    draw_up_arrow 270 545;
    draw_right_arrow 385 545;
    () 
  | 2 -> resize_window 1200 640;
    draw_background_2 magenta green cyan yellow 0.0 5 false 0.0 5 false;
    draw_left_arrow 40 545;
    draw_down_arrow 155 545;
    draw_up_arrow 270 545;
    draw_right_arrow 385 545;
    draw_left_arrow 640 545;
    draw_down_arrow 755 545;
    draw_up_arrow 870 545;
    draw_right_arrow 985 545; 
  | _ -> failwith "Invalid number of players"

(** [draw_row row i j] draws [row] of the graphcis for single player mode. *)
let rec draw_row row i j = 
  match row with 
  | [] -> ()
  | [None; Some _; Some _; Some _] -> draw_health 40 (545-(75*i))
  | [Some _; None; Some _; Some _] -> draw_health 155 (545-(75*i))
  | [Some _; Some _; None; Some _] -> draw_health 270 (545-(75*i))
  | [Some _; Some _; Some _; None] -> draw_health 385 (545-(75*i))
  | h :: t -> if h = None then draw_row t i (j+1) else begin
      match j with
      | 0 -> draw_left_arrow 40 (545-(75*i)); draw_row t i (j+1)
      | 1 -> draw_down_arrow 155 (545-(75*i)); draw_row t i (j+1)
      | 2 -> draw_up_arrow 270 (545-(75*i)); draw_row t i (j+1)
      | 3 -> draw_right_arrow 385 (545-(75*i)); draw_row t i (j+1)
      | _ -> ()
    end 

(** [update_graphics_1 matrix score lives hs] is the updated graphics 
    for single player. *)
let update_graphics_1 matrix (score: float) lives hs = 
  let rec update_graphics_1_helper matrix i = 
    match matrix with 
    | [] -> ()
    | h :: t -> draw_row h i 0; update_graphics_1_helper t (i+1); in 
  clear_graph ();
  draw_background_1 magenta green cyan yellow score lives hs;
  update_graphics_1_helper matrix 0

(** [draw_row_1 row i j] draws [row] of the graphcis 
    for player 1 in double player mode. *)
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
    end 

(** [draw_row_2 row i j] draws [row] of the graphcis 
    for player 2 in double player mode. *)
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
    end 

(** [update_graphics_2 matrix1 score1 lives1 hs1 matrix2 score2 lives2 hs2] 
    is the updated graphics for double player. *)
let update_graphics_2 matrix1 matrix2 score1 lives1 hs1 score2 lives2 hs2 = 
  let rec helper_1 matrix i = 
    match matrix with 
    | [] -> ()
    | h :: t -> draw_row_1 h i 0; helper_1 t (i+1); in 

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

(** [pause s] is the graphics screen when the game is paused. *)
let pause s = 
  set_color black;
  let x = ((size_x ()) - 200)/2 in let y = ((640-200)/2) in
  fill_rect x y 200 200;
  set_color white;
  match text_size "Paused" with
  | (a,b) -> let xp = x + (200-a)/2 in let yp = y + (200-b)/2 + 15 in 
    moveto xp yp;
    draw_string "Paused";
    match text_size "Press any key to resume" with
    | (c,d) -> let xk = x + (200-c)/2 in let yk = y + (200-d)/2 - 15 in 
      moveto xk yk;
      draw_string "Press any key to resume";
      ()

(** [quit s] is the graphics screen when the game is quit. *)
let quit s = 
  set_color black;
  let x = ((size_x ()) - 200)/2 in let y = ((640-200)/2) in
  fill_rect x y 200 200;
  set_color white;
  match text_size "Quit?" with
  | (a,b) -> let xp = x + (200-a)/2 in let yp = y + (200-b)/2 + 30 in 
    moveto xp yp;
    draw_string "Quit?";
    match text_size "Press q to quit" with
    | (c,d) -> let xk = x + (200-c)/2 in let yk = y + (200-d)/2 in 
      moveto xk yk;
      draw_string "Press q to quit";
      match text_size "Press any key to resume" with
      | (c,d) -> let xk = x + (200-c)/2 in let yk = y + (200-d)/2 -30 in 
        moveto xk yk;
        draw_string "Press any key to resume";
        ()

(** [change_color n] changes the color for drawing out the leaderboard. *)
let change_color n = 
  match (n mod 5) with
  | 1 -> set_color red;
  | 2 -> set_color green;
  | 3 -> set_color blue;
  | 4 -> set_color yellow;
  | 0 -> set_color cyan;
  | _ -> (); 
    ()

(** [leaderboard lst] draws the leaderboard. *)
let leaderboard (lst:float list) = 
  set_color magenta;
  match text_size "Leaderboard" with
  | (x,_) -> moveto ((600 - x)/2) 580; draw_string "Leaderboard";
    let rec draw_lst (lst: float list) n = 
      change_color n;
      if n > 10 then () else 
        match lst with
        | h :: t -> begin
            let str = string_of_int n ^ ". " ^ string_of_float h in
            match text_size str with
            | (x,y) -> moveto ((600 - x)/2) (580-(30*n)); draw_string str; 
              draw_lst t (n+1)
          end
        | [] -> ()
    in
    draw_lst lst 1;
    ()

(** [restart s lst] is the graphics screen when the player quits/loses/wins 
    where [s] gives the quit/lose/win message to display. *)
let restart s (lst:float list) = 
  resize_window 600 640;
  set_color black;
  fill_rect 0 0 600 640;
  leaderboard lst;
  set_color red;
  match text_size s with 
  | (x,_) -> moveto ((600-x)/2) 250; draw_string s;
    moveto 200 300;
    (draw_button "Play Again" (400/3) 150 cyan black, 
     draw_button "Quit" (800/3 + 100) 150 magenta black)

(** [restart_window s ] uses the state to call graphics updates and 
    continue the game environment *)
let restart_window s leaderboard = 
  let rec helper s = 
    match restart s leaderboard with
    | (play_again, quit) ->  
      let click = (wait_next_event [Button_down]) in 
      match play_again with 
      | (b1x,b1y) -> 
        if clicked b1x b1y click
        then "play again"
        else match quit with 
          | (b2x,b2y) -> 
            if clicked b2x b2y click
            then "quit"
            else helper s in
  helper s

