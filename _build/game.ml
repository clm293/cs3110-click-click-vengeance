open Main

(** Left is 0, Down is 1, Up is 2, Right is 3*)
type arrow = Left | Down | Up | Right

type cell = arrow option

type matrix = cell list list

type t = {
  matrix: matrix;
  score: int list;
  num_players: int;
}

let init_state = {
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
  score = [];
  num_players = 0
}

let rec make_score_list n acc =
  if n > 0 then (make_score_list (n-1) (0::acc)) else acc

let set_number_players n t = {
  matrix = t.matrix;
  score = make_score_list n [];
  num_players = n
}

let score t = t.score

let is_hit tap = failwith "unimplemented"

let generate_random_row = 
  match Random.int 4 with
  | 0 -> [Some Left; None; None; None]
  | 1 -> [None; Some Down; None; None]
  | 2 -> [None; None; Some Up; None]
  | 3 -> [None; None; None; Some Right]
  | _ -> failwith "bad row"

let update_matrix t : matrix =
  match List.rev t.matrix with
  | h :: t -> (generate_random_row)::(List.rev t)
  | _ -> failwith "bad matrix"

let update t i = failwith "unimplemented"