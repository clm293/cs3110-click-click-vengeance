(** 
   Controls all game functions.

   This module handles all scoring, music, and game data.
*)

(** The type of values representing the positions of the icons. *)
type matrix

(** The abstact type of values representing game state. *)
type t 

(** Sets the number of players for this game. *)
val set_number_players : string -> int

(** Calculates score for current state. *)
val score : t -> int

(** [is_hit c t] is whether or not the player tap is accurate. 
    A tap is accurate if it is hit at the right time and position. *)
val is_hit : Tap.t -> t -> bool

(** [update t] is the updated game state at this beat. [update] is called each
    beat.*)
val update : t -> t

(** [update_matrix t] is the updated matrix, updated the rows for each beat. 
    randomizes new icon position and sequences.*)
val update_matrix : matrix -> matrix

(** [check_hit st t] is true if the player's tap is a hit, and false otherwise*)
val check_hit : Main.inpt -> tap