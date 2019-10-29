open Yojson.Basic.Util

type difficulty = Easy | Med | Hard

type t = {
  song_name: string;
  bpm: int;
  difficulty: difficulty;
  file: string
}

let from_json json s = failwith "unimplemented"

let bpm s = failwith "unimplemented"

let difficulty s = failwith "unimplemented"

let song_name s = failwith "unimplemented"

let music_file s = failwith "unimplemented"