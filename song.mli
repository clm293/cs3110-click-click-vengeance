(**
   Representation of static song data.

   This module represents the data stored in song files. It handles loading of 
   that data from JSON as well as querying the data.
*)

type difficulty = Easy | Med | Hard

(** The abstract type of values representing songs. *)
type t

(** [from_json j] is the adventure that [j] represents.
    Requires: [j] is a valid JSON adventure representation. *)
val from_json : Yojson.Basic.t -> t

(** [bpm t] is the beats per minute of the song. *)
val bpm : t -> int

(** [difficulty t] is the difficulty of the song. *)
val difficulty : t -> difficulty 

(** [song_name t] is the name of the song. *)
val song_name : t -> string

(** [music_file t] is the file that has the music. *)
val music_file : t -> string