(** Left is 0, Down is 1, Up is 2, Right is 3*)
type arrow = Left | Down | Up | Right

type cell = arrow option

type matrix = cell list list

type press = Hit | Miss | Other

type t = {
  matrix: matrix;
  score: int; (* changed this back to an int for single player*)
  num_players: int;
  speed: float;
  scored_this_arrow : bool;
  lives_remaining: int;
  paused: bool;
  last_ten: press list
}

(** [state] is the reference pointing to the current state of the game. *)
let state = ref {
    matrix = [];
    score = 0;
    num_players = 0;
    speed = 0.0;
    scored_this_arrow = false;
    lives_remaining = 0;
    paused = false;
    last_ten = []
  }

(** [init_state num bpm] is the initial state of the game. *)
let init_state num bpm = 
  state := {
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
    speed = bpm;
    scored_this_arrow = false;
    lives_remaining = 5;
    paused = false;
    last_ten = [Miss;Miss;Miss;Miss;Miss;Miss;Miss;Miss;Miss;Miss]
  }

(** [speed ()] is the beats per second for the state's bpm *)
let speed () = 
  1.0 /. (!state.speed /. 60.0)

let rec make_score_list n acc =
  if n > 0 then (make_score_list (n-1) (0::acc)) else acc

(** [score t] is the score of state [t]. *)
let score () = !state.score

(** [get_lives] is the number of lives remaining in state [t]. *)
let get_lives () = !state.lives_remaining

(** [generate_random_row ()] is a row with an arrow in a randomly generated 
    position *)
let generate_random_row () = 
  (* if !state.score < 20 then *)
  match Random.int 4 with
  | 0 -> print_endline "0"; [Some Left; None; None; None]
  | 1 -> print_endline "1"; [None; Some Down; None; None]
  | 2 -> print_endline "2"; [None; None; Some Up; None]
  | 3 -> print_endline "3"; [None; None; None; Some Right]
  | _ -> failwith "random"
(* else 
   match Random.int 10 with 
   | 0 -> print_endline "0"; [Some Left; None; None; None]
   | 1 -> print_endline "1"; [None; Some Down; None; None]
   | 2 -> print_endline "2"; [None; None; Some Up; None]
   | 3 -> print_endline "3"; [None; None; None; Some Right]
   | 4 -> print_endline "4"; [Some Left; Some Down; None; None]
   | 5 -> print_endline "5"; [None; Some Down; Some Up; None]
   | 6 -> print_endline "6"; [None; None; Some Up; Some Right]
   | 7 -> print_endline "7"; [None; Some Down; None; Some Right]
   | 8 -> print_endline "8"; [Some Left; None; Some Up; None]
   | 9 -> print_endline "9"; [Some Left; None; None; Some Right]
   | _ -> failwith "random"*)


let is_hot lst = 
  lst = [Hit;Hit;Hit;Hit;Hit;Hit;Hit;Hit;Hit;Hit]

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
  | "" -> Other
  | _ -> Miss

(** [update_matrix t] is a matrix with all of the rows in the matrix of [t] 
    shifted down and pops off the bottom row and adds a new random row to the 
    top. *)
let update_matrix t : matrix =
  match List.rev t.matrix with
  | h :: t -> (generate_random_row ())::(List.rev t)
  | _ -> failwith "bad matrix"


(** [calc-score inpt] is the score of the game, adjusted for hits and misses. *)
let calc_score inpt = 
  let t = !state in
  if t.scored_this_arrow = true then t.score 
  else begin
    match is_hit t inpt with
    | Hit -> (if is_hot (!state.last_ten )
              then t.score +2 else t.score + 1)
    | Miss -> t.score - 1  
    | Other -> t.score
  end

(** [scored_this_arrow inpt new_score] is true if the player already scored 
    during this beat and false otherwise. *)
let scored_this_arrow inpt new_score = 
  if inpt = "beat" then  false 
  else  (if !state.scored_this_arrow = true then true else 
         if (new_score <> !state.score) then true else false)

(** [lives_remaining inpt] is the number of remaining lives the player has. *)
let lives_remaining inpt = 
  if inpt <> "beat" && (is_hit !state inpt = Miss) then
    (!state.lives_remaining -1)
  else !state.lives_remaining

let increase_speed score = 
  if (score mod 10 = 0) && (score > 0) 
  then begin print_endline "change speed"; !state.speed *. 2.0 end 
  else !state.speed

let update_graphics () = 
  if is_hot (!state.last_ten )then 
    print_endline "hotstreak";
  if !state.paused = true then () else
    Graphic.update_graphics !state.matrix !state.score !state.lives_remaining 
      (is_hot (!state.last_ten ))

(*last_ten tracks the 10 most recent press results, with the most recent 
  being the last element of the list*)
let update_last_ten (p:press) (lst: press list) = 
  match lst with
  | h::t -> List.concat [t; [p]]
  | _ -> failwith "somethings very wrong"

let pause_game () = 
  let new_state = {
    matrix = !state.matrix;
    score = !state.score;
    num_players = !state.num_players;
    speed = !state.speed;
    scored_this_arrow = !state.scored_this_arrow;
    lives_remaining = !state.lives_remaining;
    paused = true;
    last_ten = !state.last_ten
  } 
  in 
  state := new_state;
  update_graphics ()

let resume_game () = 

  let new_state = {
    matrix = !state.matrix;
    score = !state.score;
    num_players = !state.num_players;
    speed = !state.speed;
    scored_this_arrow = !state.scored_this_arrow;
    lives_remaining = !state.lives_remaining;
    paused = false;
    last_ten = !state.last_ten
  } 
  in 
  state := new_state;
  update_graphics ()



let update (inpt: string) : unit =
  if inpt = "pause" then pause_game ()
  else if inpt = "resume" then resume_game ()
  else
    let new_score = if bottom_row !state.matrix <> [None;None;None;None]
      then calc_score inpt else !state.score in
    let new_state = {
      matrix = if inpt = "beat" && (!state.paused = false) then 
          update_matrix !state else !state.matrix;
      score = new_score;
      num_players = !state.num_players;
      speed = !state.speed;
      scored_this_arrow = scored_this_arrow inpt new_score;
      lives_remaining = lives_remaining inpt;
      paused = !state.paused;
      last_ten = if inpt <> "beat" && (!state.scored_this_arrow = false) then 
          (update_last_ten (is_hit !state inpt) (!state.last_ten)) else
          !state.last_ten
    } in 

    if new_state.lives_remaining = 0 then print_endline "Game Over.";
    state := new_state;
    update_graphics ()

