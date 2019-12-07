(** 
   Controls all game functions.
   This module handles all scoring, music, and game data.
*)

(** The abstract type of values representing game state. *)
type t 

(** [leaderboard] is the leaderboard of scores from players who have played 
    during the open game session *)
val leaderboard : float list ref

(** [state] is the reference pointing to the current state of the game. *)
val state : t ref

(** [get_paused ()] is true if the game is paused and false otherwise *)
val get_paused : unit -> bool

(** [get_lives p] is the number of lives remaining for the player [p] in the 
    current state *)
val get_lives : int -> int

(** [get_beat ()] is the current beat number of the game. *)
val get_beat : unit -> int

(** [get_length ()] is the length of the level. *)
val get_length : unit -> int

(** [get_score p] is the score of player [p]. *)
val get_score : int -> float

(** [init_state num bpm len] is the initial state of a new game. *)
val init_state : int -> float -> int -> unit

(** [speed ()] is the beats per second for the state's bpm *)
val speed : unit -> float

(** [update s i] is the updated game state at this beat. *)
val update : string -> int -> unit 

(** [update_graphics ()] updates the graphics on the screen. *)
val update_graphics : unit -> unit

(** [increase_speed beat] is the new speed of the game. *)
val increase_speed : int -> float

(** [update_leaderboard score] is the new leaderboard with [score] added on. *)
val update_leaderboard : float -> unit