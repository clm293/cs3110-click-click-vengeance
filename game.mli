(** 
   Controls all game functions.
   This module handles all scoring, music, and game data.
*)
type arrow
type cell 
(** The type of values representing the positions of the icons. *)
type matrix

type press

(** The abstact type of values representing game state. *)
type t 

val leaderboard : int list ref

val state : t ref

val get_paused : unit -> bool
(** [get_lives ()] is the number of lives remaining for the player in the current
    state *)
val get_lives : int -> int

val get_beat : unit -> int

val get_length : unit -> int

val get_score : int -> int

(** [score ()] is the score for current state. *)
val score : int -> int

(** [get_curr_matrix ()] is the game matrix in state [t]. *)
val get_curr_matrix : unit -> matrix

(** [init_state num bpm] is the initial state of the game before play has started. *)
val init_state : int -> float -> int -> unit

val speed : unit -> float

(** [update t i] is the updated game state at this beat. [update] is called each
    beat.*)
val update : string -> int -> unit 

val update_graphics : unit -> unit

val increase_speed : int -> float

val update_leaderboard : int -> unit