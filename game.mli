(** 
   Controls all game functions.

   This module handles all scoring, music, and game data.
*)

(** The type of values representing the positions of the icons. *)
type matrix

type arrow

(** The abstact type of values representing game state. *)
type t 

(** [score t] is the score for current state. *)
val score : t -> int

(** [init_state num bpm] is the initial state of the game before play has started. *)
val init_state : int -> float -> t

(** [beats_per_sec st] is the beats per second for the current song. *)
val beats_per_sec : t -> float

(** [update t i] is the updated game state at this beat. [update] is called each
    beat.*)
val update : t -> string -> t