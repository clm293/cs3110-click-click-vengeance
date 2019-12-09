(**
   Representation of static level data.

   This module represents the data stored in level files. It handles loading of 
   that data from JSON as well as querying the data.
*)

type difficulty = Easy | Med | Hard

(** The abstract type of values representing levels. *)
type t

(** [from_json j] is the level that [j] represents.
    Requires: [j] is a valid JSON song representation. *)
val from_json : Yojson.Basic.t -> t

(** [bpm t] is the initial beats per minute of the level. *)
val bpm : t -> float

(** [difficulty t] is the difficulty of the level. *)
val difficulty : t -> difficulty 

val length : t -> int 
