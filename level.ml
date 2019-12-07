open Yojson.Basic.Util

type difficulty = Easy | Med | Hard

type t = {
  bpm: float;
  difficulty: difficulty;
  length: int;
}

let diff_from_json json = 
  match json |> member "difficulty" |> to_string with
  | "easy" -> Easy
  | "medium" -> Med
  | "hard" -> Hard
  | _ -> failwith "bad song difficulty"

let from_json json = {
  bpm = json |> member "bpm" |> to_float;
  difficulty = diff_from_json json;
  length = json |> member "length" |> to_int;
}

let bpm s = s.bpm

let difficulty s = s.difficulty

let length s =  s.length


