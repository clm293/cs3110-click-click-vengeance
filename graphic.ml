open Graphics 

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

let draw_background c1 c2 c3 c4 score = 
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
  set_text_size 100;
  moveto 500 610;
  draw_string ("Score: " ^ (string_of_int score));
  ()

let draw_left_arrow x y = 
  draw_image (make_image (create_left_arrow_matrix black transp)) x y;
  ()

let draw_down_arrow x y = 
  draw_image (make_image (create_down_arrow_matrix black transp)) x y;
  ()

let draw_up_arrow x y = 
  draw_image (make_image (create_up_arrow_matrix black transp)) x y;
  ()

let draw_right_arrow x y = 
  draw_image (make_image (create_right_arrow_matrix black transp)) x y;
  ()

let init_graphics s st = 
  open_graph s;
  resize_window 600 640;
  set_window_title "Tap Tap Revenge Game";
  draw_background magenta green cyan yellow 0;
  draw_left_arrow 40 545;
  draw_down_arrow 155 545;
  draw_up_arrow 270 545;
  draw_right_arrow 385 545;
  ()

let update_graphics matrix score = 
  let rec draw_row row i j = 
    match row with 
    | [] -> ()
    | h :: t -> if h = None then draw_row t i (j+1) else begin
        match j with
        | 0 -> draw_left_arrow 40 (545-(75*i));
        | 1 -> draw_down_arrow 155 (545-(75*i));
        | 2 -> draw_up_arrow 270 (545-(75*i));
        | 3 -> draw_right_arrow 385 (545-(75*i));
        | _ -> ()
      end in
  let rec helper matrix i = 
    match matrix with 
    | [] -> ()
    | h :: t -> draw_row h i 0; helper t (i+1); in 
  clear_graph ();
  draw_background magenta green cyan yellow score;
  helper matrix 0