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

val state : t ref

val get_paused : unit -> bool
(** [get_lives (player number)] is the number of lives remaining for the player in the current
    state *)
val get_lives : int -> int

(** [get_num_players] is the number of players in the game state *)
val get_num_players : unit -> int

(** [score (player number)] is the score for the player in the current state. *)
val score : int -> int

(** [get_curr_matrix ()] is the game matrix in state [t]. *)
val get_curr_matrix : unit -> matrix

(** [get_beat] is the beat number of the game *)
val get_beat : unit -> int

(** [get_length] is the length of the level*)
val get_length : unit -> int

(** [init_state num bpm] is the initial state of the game before play has started. *)
val init_state : int -> float -> int -> unit

val speed : unit -> float

(** [update inpt player_num] is the updated game state at this call. [update] is 
    called each on beat and each keyboard input.*)
val update : string -> int -> unit 

val update_graphics : unit -> unit

val increase_speed : int -> float