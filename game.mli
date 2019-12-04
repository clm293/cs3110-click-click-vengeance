(** 
   Controls all game functions.
   This module handles all scoring, music, and game data.
*)

(** The abstract type of values representing game state. *)
type t 

(** [leaderboard] is the leaderboard of scores from players who have played 
    during the open game session *)
val leaderboard : int list ref

(** [state] is the reference pointing to the current state of the game. *)
val state : t ref

(** [get_paused ()] is true if the game is paused and false otherwise *)
val get_paused : unit -> bool

(** [get_lives ()] is the number of lives remaining for the player in the current
    state *)
val get_lives : int -> int

val get_beat : unit -> int

val get_length : unit -> int

val get_score : int -> int

(** [init_state num bpm] is the initial state of the game before play has started. *)
val init_state : int -> float -> int -> unit

val speed : unit -> float

(** [update t i] is the updated game state at this beat. [update] is called each
    beat.*)
val update : string -> int -> unit 

val update_graphics : unit -> unit

val increase_speed : int -> float

val update_leaderboard : int -> unit