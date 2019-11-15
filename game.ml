(** Left is 0, Down is 1, Up is 2, Right is 3*)
type arrow = Left | Down | Up | Right

type cell = arrow option

type matrix = cell list list

type press = Hit | Miss | Other 

type t = {
  matrix: matrix;
  score: int; (* changed this back to an int for single player*)
  num_players: int;
  bpm: float;
}

let init_state num bpm = {
  matrix = [
    [None; None; None; None];
    [None; None; None; None];
    [None; None; None; None];
    [None; None; None; None];
    [None; None; None; None]; 
    [None; None; None; None];
    [None; None; None; None];
    [None; None; None; None];
  ];
  score = 0; (* changed this back to an int for single player*)
  num_players = num;
  bpm = bpm
}

let beats_per_sec st = 
  st.bpm /. 60.0

let rec make_score_list n acc =
  if n > 0 then (make_score_list (n-1) (0::acc)) else acc

let score t = t.score

(** [generate_random_row ()] is a row with an arrow in a randomly generated 
    position *)
let generate_random_row () = 
  match Random.int 4 with
  | 0 -> print_endline "0"; [Some Left; None; None; None]
  | 1 -> print_endline "1"; [None; Some Down; None; None]
  | 2 -> print_endline "2"; [None; None; Some Up; None]
  | 3 -> print_endline "3"; [None; None; None; Some Right]
  | _ -> failwith "bad row"

(** [bottom_row m] is the bottom row of the matrix [m] *)
let bottom_row m = 
  match List.rev m with
  | h :: t -> h
  | _ -> failwith "bad matrix"

(** [is_hit t inpt] is whether or not the player's tap is accurate. 
    A tap is accurate if it is hit at the correct time and position. *)
let is_hit t inpt = 
  match inpt with
  | "up" -> if List.mem (Some Up) (bottom_row t.matrix) then Hit else Miss
  | "down" -> if List.mem (Some Down) (bottom_row t.matrix) then Hit else Miss
  | "left" -> if List.mem (Some Left) (bottom_row t.matrix) then Hit else Miss
  | "right" -> if List.mem (Some Right) (bottom_row t.matrix) then Hit else Miss
  | _ -> failwith "bad key"

(** [update_matrix t] is a matrix with all of the rows in the matrix of [t] 
    shifted down and pops off the bottom row and adds a new random row to the 
    top. *)
let update_matrix t : matrix =
  match List.rev t.matrix with
  | h :: t -> (generate_random_row ())::(List.rev t)
  | _ -> failwith "bad matrix"

(** [calc-score t inpt] is the score of the game, adjusted for hits and misses. *)
let calc_score t inpt = 
  if is_hit t inpt = Hit then t.score + 1 else t.score

let get_matrix t = t.matrix

let update t = 

  let new_state = {
    matrix = update_matrix t;
    score = t.score;
    num_players = t.num_players;
    bpm = t.bpm
  } in 
  Graphic.update_graphics (new_state.matrix);
  new_state

let update_score t inpt = 
  let new_state = {
    matrix =  t.matrix;
    score = calc_score t inpt;
    num_players = t.num_players;
    bpm = t.bpm
  } in 
  update new_state




let speed bpm = failwith "unimplemented"