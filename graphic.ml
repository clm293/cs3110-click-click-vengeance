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

let init_graphics s st = 
  open_graph s;
  resize_window 500 680;
  set_window_title "Tap Tap Revenge Game";
  set_color black;
  fill_rect 0 0 500 680;
  set_color magenta;
  fill_rect 20 20 115 640;
  set_color yellow;
  fill_rect 135 20 115 640;
  set_color green;
  fill_rect 250 20 115 640;
  set_color cyan;
  fill_rect 365 20 115 640;
  set_line_width 5;
  set_color black;
  draw_rect 30 30 440 75;

  draw_image (make_image (create_left_arrow_matrix black magenta)) 40 565;
  draw_image (make_image (create_down_arrow_matrix black yellow)) 155 565;
  draw_image (make_image (create_up_arrow_matrix black green)) 270 565;
  draw_image (make_image (create_right_arrow_matrix black cyan)) 395 565;
  ()