(** 
   Controls all game functions.

   This module handles all scoring, music, and game data.
*)

(** The type of values representing the positions of the icons. *)
type matrix

type arrow

(** The abstact type of values representing game state. *)
type t 

val state : t ref

(** [get_lives t] is the number of lives remaining for the player in the current
    state *)
val get_lives : t -> int

(** [score t] is the score for current state. *)
val score : t -> int

(** [init_state num bpm] is the initial state of the game before play has started. *)
val init_state : int -> float -> unit

(** [beats_per_sec st] is the beats per second for the current song. *)
val beats_per_sec : unit -> float

(** [update t i] is the updated game state at this beat. [update] is called each
    beat.*)
val update : string -> unit 

val update_graphics : unit -> unit

(* val update_score : t -> string -> t *)