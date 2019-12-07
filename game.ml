open Graphics
open Graphic

(** [arrow] is the type of values representing an arrow on the screen. *)
type arrow = Left | Down | Up | Right

(** The type of values representing a cell in the game matrix. *)
type cell = arrow option

(** The type of values representing the game matrix that stores data
    about the arrows on the screen. *)
type matrix = cell list list

(** The type of values representing a player key press. *)
type press = Hit | Miss | Other

(** The type of values representing a player. *)
type player = {
  score: int;
  scored_this_arrow : bool;
  lives_remaining: int;
  last_ten: press list;
  first_of_double: string;
}

type t = {
  matrix: matrix;
  num_players: int;
  speed: float;
  paused: bool;
  length: int;
  beat: int;
  players: player * player option;
}

let leaderboard = ref []

(** [player_1_ref] is the reference to the state of player 1. *)
let player_1_ref = ref {
    score = 0;
    scored_this_arrow = false;
    lives_remaining = 5;
    last_ten = [Miss;Miss;Miss;Miss;Miss;Miss;Miss;Miss;Miss;Miss];
    first_of_double = "";
  }

(** [player_2_ref] is the reference to the state of player 2 *)
let player_2_ref = ref {
    score = 0;
    scored_this_arrow = false;
    lives_remaining = 5;
    last_ten = [Miss;Miss;Miss;Miss;Miss;Miss;Miss;Miss;Miss;Miss];
    first_of_double = "";
  } 

let state = ref {
    matrix = [];
    num_players = 0;
    speed = 0.0;
    paused = false;
    length = 0;
    beat = 0;
    players = (!player_1_ref, None);
  }

(** [init_player p] is the initial state of a player. *)
let init_player p =
  if p = 1 then player_1_ref := {
      score = 0;
      scored_this_arrow = false;
      lives_remaining = 5;
      last_ten = [Miss;Miss;Miss;Miss;Miss;Miss;Miss;Miss;Miss;Miss];
      first_of_double = "";
    }
  else player_2_ref := {
      score = 0;
      scored_this_arrow = false;
      lives_remaining = 5;
      last_ten = [Miss; Miss; Miss; Miss; Miss; Miss; Miss; Miss; Miss; Miss];
      first_of_double = "";
    }

let init_state num bpm len = 
  print_endline "init_state";
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
    num_players = num;
    speed = bpm;
    paused = false;
    length = len;
    beat = 0;
    players = if num = 1 then begin init_player 1; (!player_1_ref, None) end 
      else 
        begin 
          init_player 1; init_player 2; 
          (!player_1_ref, Some !player_2_ref) 
        end;
  }

let get_paused () = !state.paused

let speed () = 
  1.0 /. (!state.speed /. 60.0)

let get_lives player = if player = 1 then !player_1_ref.lives_remaining 
  else !player_2_ref.lives_remaining

let get_beat () = !state.beat

let get_length () = !state.length

let get_score player =
  if player = 1 then !player_1_ref.score else !player_2_ref.score

(** [single_rows] is a list of the possibilities of rows with a single arrow. *)
let single_rows = [
  [Some Left; None; None; None];
  [None; Some Down; None; None];
  [None; None; Some Up; None];
  [None; None; None; Some Right]
]

(** [single_rows] is a list of the possibilities of rows with two arrows. *)
let double_rows = [
  [Some Left; Some Down; None; None];
  [None; Some Down; Some Up; None];
  [None; None; Some Up; Some Right];
  [None; Some Down; None; Some Right];
  [Some Left; None; Some Up; None];
  [Some Left; None; None; Some Right]
]

(** [generate_random_row ()] is a row with an arrow in a randomly generated 
    position *)
let generate_random_row () = 
  if !state.beat < 10 then
    match Random.int 5 with
    | 0 -> [Some Left; None; None; None]
    | 1 -> [None; Some Down; None; None]
    | 2 -> [None; None; Some Up; None]
    | 3 -> [None; None; None; Some Right]
    | 4 -> [None; None ; None; None]
    | _ -> failwith "generate random row error"
  else 
    match Random.int 11 with 
    | 0 -> [Some Left; None; None; None]
    | 1 -> [None; Some Down; None; None]
    | 2 -> [None; None; Some Up; None]
    | 3 -> [None; None; None; Some Right]
    | 4 -> [Some Left; Some Down; None; None]
    | 5 -> [None; Some Down; Some Up; None]
    | 6 -> [None; None; Some Up; Some Right]
    | 7 -> [None; Some Down; None; Some Right]
    | 8 -> [Some Left; None; Some Up; None]
    | 9 -> [Some Left; None; None; Some Right]
    | 10 -> [None; None; None; None]
    | _ -> failwith "generate random row error"

(** [is_hot lst] is true if the previous 10 presses were hits. *)
let is_hot lst = 
  lst = [Hit; Hit; Hit; Hit; Hit; Hit; Hit; Hit; Hit; Hit]

(** [bottom_row m] is the bottom row of the matrix [m] *)
let bottom_row m = 
  match List.rev m with
  | h :: t -> h
  | _ -> failwith "invalid matrix"

(** [is_double_hit sec_inpt player] is true if a hit for a double key press was
    made. *)
let is_double_hit sec_inpt player = 
  let t = !state in
  if not (List.mem (!player.first_of_double) ["right";"left";"up";"down"]) 
  then Miss else
    let fst_inpt = !player.first_of_double in 
    match fst_inpt, sec_inpt with 
    | "up", "left" -> if List.mem (Some Up)(bottom_row t.matrix) && 
                         List.mem (Some Left )(bottom_row t.matrix) 
      then Hit else Miss
    | "left", "up" -> if List.mem (Some Up)(bottom_row t.matrix) && 
                         List.mem (Some Left )(bottom_row t.matrix) 
      then Hit else Miss
    | "up", "right" -> if List.mem (Some Up)(bottom_row t.matrix) && 
                          List.mem (Some Right )(bottom_row t.matrix) 
      then Hit else Miss
    | "right", "up" -> if List.mem (Some Up)(bottom_row t.matrix) && 
                          List.mem (Some Right )(bottom_row t.matrix) 
      then Hit else Miss
    | "up", "down" -> if List.mem (Some Up)(bottom_row t.matrix) && 
                         List.mem (Some Down )(bottom_row t.matrix) 
      then Hit else Miss
    | "down", "up" -> if List.mem (Some Up)(bottom_row t.matrix) && 
                         List.mem (Some Down )(bottom_row t.matrix) 
      then Hit else Miss
    | "left", "down" -> if List.mem (Some Left)(bottom_row t.matrix) && 
                           List.mem (Some Down )(bottom_row t.matrix) 
      then Hit else Miss
    | "down", "left" -> if List.mem (Some Left)(bottom_row t.matrix) && 
                           List.mem (Some Down )(bottom_row t.matrix) 
      then Hit else Miss
    | "right", "down" -> if List.mem (Some Right)(bottom_row t.matrix) && 
                            List.mem (Some Down )(bottom_row t.matrix) 
      then Hit else Miss
    | "down", "right" -> if List.mem (Some Right)(bottom_row t.matrix) && 
                            List.mem (Some Down )(bottom_row t.matrix) 
      then Hit else Miss
    | "left", "right" -> if List.mem (Some Right)(bottom_row t.matrix) && 
                            List.mem (Some Left )(bottom_row t.matrix) 
      then Hit else  Miss
    | "right", "left" -> if List.mem (Some Right)(bottom_row t.matrix) && 
                            List.mem (Some Left )(bottom_row t.matrix) 
      then Hit else Miss
    | "", _ -> Other
    | _, "" -> Other
    | _ -> Miss

(** [is_hit t inpt] is whether or not the player's tap is accurate. 
    A tap is accurate if it is hit at the correct time and position. *)
let is_hit inpt player = 
  if List.mem (bottom_row (!state.matrix)) double_rows 
  then is_double_hit inpt player else 
    match inpt with
    | "up" -> if List.mem (Some Up) (bottom_row !state.matrix) 
      then Hit else Miss
    | "down" -> if List.mem (Some Down) (bottom_row !state.matrix) 
      then Hit else Miss
    | "left" -> if List.mem (Some Left) (bottom_row !state.matrix) 
      then Hit else Miss
    | "right" -> if List.mem (Some Right) (bottom_row !state.matrix) 
      then Hit else Miss
    | "" -> Other
    | _ -> Miss

(** [update_matrix t] is a matrix with all of the rows in the matrix of [t] 
    shifted down and pops off the bottom row and adds a new random row to the 
    top. *)
let update_matrix () =
  match List.rev !state.matrix with
  | h :: t -> (generate_random_row ())::(List.rev t)
  | _ -> failwith "bad matrix"

(** [remove_last_three m] removes the last rows of the matrix [m]. *)
let rec remove_last_three m = 
  if List.length m = 5 then m else 
    match m with
    | h :: t -> remove_last_three t
    | _ -> failwith "invalid matrix"

(** [resume_matrix m acc] is the game matrix after a player resumes from
    pause.*)
let rec resume_matrix m acc = 
  [[None;None;None;None];
   [None;None;None;None];
   [None;None;None;None]] 
  |> List.rev_append (List.rev (remove_last_three m)) 

(** [calc-score inpt] is the score of the game, adjusted for hits and misses. *)
let calc_score inpt player = 
  if !player.scored_this_arrow  then !player.score 
  else if !state.paused then !player.score
  else 
    begin
      match is_hit inpt player with
      | Hit -> (if is_hot (!player.last_ten )
                then !player.score + 2 else !player.score + 1)
      | Miss -> !player.score
      | Other -> !player.score
    end

(** [scored_this_arrow inpt new_score] is true if the player already scored 
    during this beat and false otherwise. *)
let scored_this_arrow inpt new_score player = 
  if inpt = "beat" then false 
  else (if player.scored_this_arrow = true then true else 
        if (new_score <> player.score) then true else false)

(** [lives_remaining inpt] is the number of remaining lives the player has. *)
let lives_remaining inpt new_matrix p = 
  let player = if p = 1 then player_1_ref else player_2_ref in
  if (List.mem (bottom_row new_matrix) double_rows) &&
     !player.first_of_double = "" then !player.lives_remaining 
  else (if inpt <> "beat" && (is_hit inpt player = Miss) then
          !player.lives_remaining - 1 else !player.lives_remaining)

let increase_speed beat = 
  if (beat mod 20 = 0) then !state.speed *. 1.1 else !state.speed

let update_leaderboard score = 
  leaderboard := (score::!leaderboard) 
                 |> List.sort compare
                 |> List.rev

let update_graphics () = 
  if !state.num_players = 1 then 
    begin 
      if !state.paused = true then ()
      else Graphic.update_graphics_1 !state.matrix !player_1_ref.score 
          !player_1_ref.lives_remaining (is_hot (!player_1_ref.last_ten)) 
    end
  else if !state.paused = true then () 
  else 
    begin 
      Graphic.update_graphics_2 !state.matrix !player_1_ref.score 
        !player_1_ref.lives_remaining (is_hot (!player_1_ref.last_ten)) 
        !player_2_ref.score 
        !player_2_ref.lives_remaining (is_hot (!player_2_ref.last_ten)) 
    end

(* [update_last_ten p lst] is the 10 most recent press results, with the most 
   recent being the last element of the list*)
let update_last_ten (p:press) (lst: press list) = 
  match lst with
  | h::t -> List.concat [t; [p]]
  | _ -> failwith "invalid list"

(** [pause_game ()] pauses the game. *)
let pause_game beat = 
  let new_state = {
    matrix = !state.matrix;
    num_players = !state.num_players;
    speed = !state.speed;
    paused = true;
    length = !state.length;
    beat = beat;
    players = !state.players;
  } 
  in 
  state := new_state;
  update_graphics ()

(** [resume_game ()] resumes the game after being paused. *)
let resume_game beat = 
  let new_state = {
    matrix = resume_matrix !state.matrix [];
    num_players = !state.num_players;
    speed = !state.speed;
    paused = false;
    length = !state.length;
    beat = beat;
    players = !state.players;
  } 
  in 
  state := new_state;
  update_graphics ()

(** [update_player i m p] updates the player state. *)
let rec update_player inpt matrix p = 
  let player = (if p = 1 then player_1_ref else player_2_ref) in
  let new_score = if bottom_row !state.matrix <> [None;None;None;None]
    then calc_score inpt player else !player.score in
  let new_player_state = {
    score = new_score;
    scored_this_arrow = scored_this_arrow inpt new_score !player;
    lives_remaining = lives_remaining inpt matrix p;
    last_ten = if inpt <> "beat" && (!player.scored_this_arrow = false) 
                  && not (List.mem (bottom_row matrix) double_rows && 
                          !player.first_of_double = "") then 
        (update_last_ten (is_hit inpt player) (!player.last_ten)) else
        !player.last_ten;
    first_of_double = if List.mem (bottom_row matrix) double_rows &&
                         inpt <> "beat" then inpt else "";
  } in 
  player := new_player_state

let rec update (inpt: string) (plyr: int): unit =
  if !state.paused = true then 
    if inpt = "resume" then resume_game !state.beat else pause_game !state.beat 
  else
  if inpt = "pause" then pause_game !state.beat else
    let new_matrix = if inpt = "beat" && (!state.paused = false) then 
        update_matrix () else !state.matrix in
    if inpt = "beat" then 
      begin 
        update_player inpt new_matrix 1; 
        update_player inpt new_matrix 2 
      end
    else
      update_player inpt new_matrix plyr;
    let new_state = {
      matrix = new_matrix;
      num_players = !state.num_players;
      speed = if inpt = "beat" then increase_speed (!state.beat + 1) 
        else !state.speed;
      paused = !state.paused;
      length = !state.length;
      beat = if inpt = "beat" then !state.beat + 1 else !state.beat;
      players = !state.players
    } 
    in 
    print_endline (string_of_int new_state.beat);
    state := new_state;
    update_graphics () 
